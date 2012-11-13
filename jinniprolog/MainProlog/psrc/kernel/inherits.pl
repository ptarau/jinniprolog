oconsult(F):-
  rollback,
  scan_super(F,assert,Ys),
  process_inheritance_data(Ys,Goals,FNs),
  process_constructors(F,FNs,assert),
  call_all_goals(Goals).
  
ocompile(F):-rollback,ocompile(F,mem).
 
odebug(F):-rollback,ocompile(F,debug).

ocompile(F,Mode):-
  symcat('$end',F,EF),
  advance_code_top,
  scan_super(F,cc(Mode),Ys),
  process_inheritance_data(Ys,Goals,Supers),
  process_constructors(F,Supers,cc(Mode)),
  jterminate(EF,Mode),
  call_all_goals(Goals).

ocheck(F):-
  rollback,
  atom(F),
  namecat('db_',F,DF),
  db_clean(DF),
  ClauseOP=db_assert(DF),
  scan_super(F,ClauseOP,Ys),
  process_inheritance_data(Ys,Goals,FNs),
  process_constructors(F,FNs,ClauseOP),
  call(ClauseOP,'$goals'(Goals)),
  %db_listing(DF),
  oanalyze(DF),
  db_clean(DF).

oanalyze(DF):-
  oclassify_head(DF,H,Type),
  Type==clashing,
  pp_clause(definition_clash=>H),
  fail.
oanalyze(DF):-
  oclassify_body(DF,H,B,Type),
  ( Type==undefined->pp_clause(undefined_or_not_yet_asserted_in(H)=>B)
  ; Type==defined_uncallable->pp_clause(mathing_no_local_definition_in(H)=>B)
  ),
  fail.
oanalyze(DF):-
  println(finished_analizing(DF)).
     
oclassify_head(DF,H,Type):-
  db_clause(DF,H,_),
  ( is_compiled(H)->Type=clashing
  ; Type=can_be_defined
  ).
   
oclassify_body(DF, H,B,Type):-
  db_clause(DF,H,Bs),
  in_body(Bs,B),
  oclassify_call(DF,B,Type).

oclassify_call(DF,B,Type):-
  db_clause(DF,B,_Bs),
  !,
  Type=defined_callable.
oclassify_call(DF,B,Type):-
  functor(B,F,N),functor(H,F,N),
  db_clause(DF,H,_Bs),
  !,
  Type=defined_uncallable.
oclassify_call(_,B,Type):-
  classify_pred(B,Type).
  
call_all_goals(Goals):-
  foreach(member(G,Goals),topcall(G)).
   
process_inheritance_data(Data,Goals,Supers):-
  findall(G,(member(in_class(_Fname,_File,_Is,_Ps,Gs),Data),member(G,Gs)),Goals),
  findall(F/N,(member(in_class(F,_File,_Is,Ps,_Gs),Data),member(F/N,Ps)),Supers).

% calls Action on clauses synthetised from constructors in this class and supers
process_constructors(ThisF,FNs,Action):-
  S='$super',
  /* DEPRECATED !!!
  % calls chain of arity 0 constructors
  % this imitates Java but is really unneded as we
  % ensure that constructors of _any_ arity are inherited
  ( member(F/0,FNs),
      F\==ThisF,
      call(Action,(S:-F,fail)),
    fail
  ; true
  ),
  */
  call(Action,(S:-true)),
  foldl(max_of_2_arities,_/0,FNs,_/MaxN),
  reverse(FNs,NewFNs),
  ( member(ThisF/0,NewFNs)->true
  ; call(Action,(ThisF:-true))
  ),
  ( for(N,0,MaxN), % now arity 0 is treated the same as all others
      \+(member(ThisF/N,NewFNs)),
      supply_constructor(ThisF/N,NewFNs,Action),
    fail
  ; true
  ).
  
max_of_2_arities(_/N1,_/N2,_/N):-max(N1,N2,N).

supply_constructor(ThisF/N,NewFNs,Action):-
  member(F/N,NewFNs),
  !,
  build_a_constructor(ThisF,F,N,Clause),
  call(Action,Clause).

build_a_constructor(ThisF,F,N,(H:-B)):-
  length(Xs,N),
  H=..[ThisF|Xs],
  B=..[F|Xs].
  
scan_super(F,Ys):-
  scan_super(F,eq(_),Ys).

/*

  recursively reads a number of files and their includes
  and appyies ClauseOP to each clause
  while collecting date about predicates and their locations
  as well as goal and constructors to be executed later
  
*/    
scan_super(F_or_Fs,ClauseOP,Ys):-
  if(F_or_Fs=[_|_],Fs=F_or_Fs,Fs=[F_or_Fs]),
  scan_super(Fs,ClauseOP,[],Ys).
      
scan_super([],_ClauseOP,Xs,Xs).
scan_super([Fname|Fs],ClauseOP,SoFar,Final):-
  includes_preds_clause_stream_of(Fname,File,Is,Ps,ClauseS,Gs),
  apply_clause_op(ClauseOP,ClauseS,SoFar),
  findall(I,not_seen_file(I,Is,SoFar),NewIs),
  findall(P,not_seen_pred(P,Ps,SoFar),NewPs),
  det_append(NewIs,Fs,NewFs),
  scan_super(NewFs,ClauseOP,[in_class(Fname,File,NewIs,NewPs,Gs)|SoFar],Final).

apply_clause_op(ClauseOP,ClauseS,SoFar):-
  good_element_of(ClauseS,C0),
  to_clause(C0,C),
  C=(H:-_),functor(H,F,N),
  \+(pred_already_seen(F/N,SoFar)),
  call(ClauseOP,C),
  fail.
apply_clause_op(_,_,_).

not_seen_file(I,Is,SoFar):-
  member(I,Is),
  \+(file_already_included(I,SoFar)).

not_seen_pred(P,Ps,SoFar):-
  member(P,Ps),
  \+(pred_already_seen(P,SoFar)).

file_already_included(Fname,SoFar):-member(in_class(Fname,_File,_Is,_FNs,_Gs),SoFar).
pred_already_seen(FN,SoFar):-member(in_class(_Fname,_File,_Is,FNs,_Gs),SoFar),member(FN,FNs).

includes_preds_clause_stream_of(Fname,File,Is,FNs,ClauseS,Gs):-
  find_file(Fname,File),
  file_clause_reader(File,Reader),
  split_source(Reader,PredS,IncludeS,ClauseS),
  preds_of_reader(PredS,FNs),
  includes_and_inits_of_reader(IncludeS,Is,Gs).
  
includes_and_inits_of_reader(IncludeS,Fs,Gs):-
  GTerm=':-'(Goal),
  findall(Goal,good_element_of(IncludeS,GTerm),Directives),
  findall(F,(member(D,Directives),D=[I|Is],member(F,[I|Is])),Fs),
  findall(G,(member(D,Directives),D=initialization(G)),Gs).

/*
  todo: :-initialization(Goal). to be handled like in ISO Prolog
  after the (complete) inhertance tree is loaded
  
*/

itest:-scan_super(vtetris,Xs),
    forall(member(X,Xs),println(X)).
/*
iconsult(F):-
  scan_super(F,assert,Ys),
  process_inheritance_data(F,Ys,Gs,FNs),
  process_constructors(FNs,F,assert),
  forall(member(G,Gs),topcall(G)).
*/

icompile(F):-icompile(F,mem).

icompile(F,Mode):-
  advance_code_top,
  scan_super(F,cc(Mode),Ys),
  process_inheritance_data(F,Ys,Gs,FNs,Arities),
  process_constructors(Arities,FNs,F,cc(Mode)),
  jterminate(F,Mode),
  forall(member(G,Gs),topcall(G)).

process_inheritance_data(ThisF,Data,Goals,Supers,Arities):-
  findall(G,(member(in_class(_Fname,_File,_Is,_Ps,Gs),Data),member(G,Gs)),Goals),
  findall(F/N,(member(in_class(F,_File,_Is,Ps,_Gs),Data),member(F/N,Ps),F\=ThisF),Supers),
  findall(N,(member(in_class(ThisF,_File,_Is,Ps,_Gs),Data),member(ThisF/N,Ps)),Arities).
  
process_constructors(Ns,FNs,Action):-
  member(N,Ns),
  member(F/N,FNs),
  make_super(F/N,C),
  call(Action,C)
; 
  functor(T,'$super',N),
  call(Action,(T:-true)).
  
make_super(F/N,(H:-B,fail)):-
  length(Xs,N),
  H=..['$super'|Xs],
  B=..[F|Xs].
  
scan_super(F,Ys):-
  scan_super(F,eq(_),Ys).
    
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
  element_of(ClauseS,C0),
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
  findall(Goal,element_of(IncludeS,GTerm),Directives),
  findall(F,(member(D,Directives),D=[I|Is],member(F,[I|Is])),Fs),
  findall(G,(member(D,Directives),D=initialization(G)),Gs).

t1:-
  qsort:qsort([b,a,c,d,a],S),
  println(S),
  nrev:nrev(S,R),
  println(R),
  qsort:assert(a(1)),
  nrev:assert(a(2)),
  qsort:assert(a(3)),
  nrev:a(X),
  forall(
    qsort:a(Y),
    println(first=Y)
  ),
  println(second=X).


% seems to happen as a result of a diff between mjc.bat and mjinni.bat

ok1:-find_file('http://daisy:81/jinnidemo/progs/hello.pl',F),println(F).


bug1:-see('../progs/hello.pl'),repeat,read(X),println(X),X=end_of_file,!,seen.

bug2:-see('http://daisy:81/jinnidemo/progs/hello.pl'),repeat,read(X),println(X),X=end_of_file,!,seen.

ok2:-term_of('http://daisy:81/jinnidemo/progs/hello.pl',T),println(T),fail.

t2:-
  new_char_reader('http://www.binnetcorp.com/download/jinnidemo/progs/nrev.pl',Reader),
  forall(element_of(Reader,C),put(C)).

t3:-
  'http://www.binnetcorp.com/download/jinnidemo/progs/nrev.pl':go.

t4:-
  'http://www.binnetcorp.com/download/jinnidemo/progs/nrev':go.

t5:-
  rollback,
  qcompile('../progs/nrev.pl'),go.

t6:-
  ['http://www.binnetcorp.com/download/jinnidemo/progs/nrev.pl'],go.

t7:-
  term_of('http://www.binnetcorp.com/download/jinnidemo/progs/nrev.pl',T),
  println(T),
  !.

t8:-
  late_definition(X),
  println(X),
  fail.




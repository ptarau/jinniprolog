% indexed dynamic database layer
% also contains custom made metainterpreter

% clause table

iclauses_clean(Db):-
  iclause_iterate(Db,Ref),
  iclause_rm(Db,Ref),
  fail.
iclauses_clean(Db):-  
  init_gensym(Db).

new_iclause(Db,HB,Ref):-
  gensym_no(Db,Ref),
  def(Db,Ref,HB).
 
iclause_get(Db,Ref,HB):-
  val(Db,Ref,HB).

iclause_rm(Db,Ref):-
  val(Db,Ref,_),
  rm(Db,Ref).
  
iclause_all(Db,Ref,HB):-
  iclause_iterate(Db,Ref),
  val(Db,Ref,HB).

iclause_iterate(Db,Ref):-
  val(gensym,Db,Last),
  for(Ref,1,Last).
    
iclauses_show(Db):-
  iclause_all(Db,_,HB),
  pp_clause(HB),
  fail.
iclauses_show(_). 

% discovery of indexable args

arg_path_of(H,I,G):-
  argn(I,H,A),
  % I<3,
  \+float(A),
  functor(A,G,_).

% discovery of non-indexable args

var_path_of(H,I):-
  argn(I,H,A),
  var(A).
 
% the dictionary of dictionaries

root_dict(Dict):-named_dict('$root',Dict).
  
named_dict(DictName,Dict):-val('$dict',DictName,Dict),!.
named_dict(DictName,Dict):-new_dict(Dict),def('$dict',DictName,Dict).

% indexing tools

% if K is known, checks if it is a key, if not generates them all
dict_key(D,K):-ground(K),!,dict_get(D,K,V),V\=='$null'.
dict_key(D,K):-
  invoke_java_method(D,getKeys,Iter),
  iterator_element(Iter,K).

% enumerates on backtrackin all children in a dictionary  
dict_child_of(ParentDict,DbName,Dict):-
  dict_key(ParentDict,DbName),
  % this will not insert a new child as 
  % dict_key only enumerates existing
  dict_get(ParentDict,DbName,Dict).  

% adding to the index
idb_index(Db,HB,Ref):-
  Locked= -1,
  ( nonvar(Db),HB=(H:-_),functor(H,F,N)->
    true
  ; !,errmes(instantiation_error,adding_to(Db,Ref:HB))
  ),
  root_dict(RD),
  dict_ensure_child(RD,Db,D),
  dict_ensure_child(D,F/N,DFN),
  dict_ensure_child(DFN,0,DF0),
  dict_put(DF0,Ref,0),
  ( arg_path_of(H,I,G),
      (dict_get(DFN,I,Val),Val==Locked->fail;true),
      dict_ensure_child(DFN,I,DFI),
      dict_ensure_child(DFI,G,DG), % G->G/M
      dict_put(DG,Ref,I),
    fail
  ;
    var_path_of(H,I),
      % lock index position on first var arg
      (dict_get(DFN,I,DFI)->Found=true;Found=fail),
      dict_put(DFN,I,Locked),
      % (Found->delete_java_object(DFI);true), % $$ bug in new_dict?
    fail
  ; 
    true
  ).

% retrieval from the index
idb_get_index(DbName,(H:-_),Ref):-
  Locked= -1,
  root_dict(RD),
  dict_child_of(RD,DbName,D),
   % tab(2),println(trace_entering_db(DbName,H)),
   % tab(2),object_to_string(D,SD),println(SD),
  (functor(H,F,N)->true;true),
  dict_child_of(D,F/N,DFN),
  (var(H)->functor(H,F,N);true),
    % tab(4),println(trace_entering_pred(F/N=>DFN)),
    % tab(4),object_to_string(DFN,SDFN),println(SDFN),
  ( arg_path_of(H,I,G),
      % tab(6),println(trace_entering_arg_path(F/N=>I:G)),
    dict_child_of(DFN,I,DFI),
    DFI\==Locked
    ->
      % tab(6),println(trace_finding_arg(i=I,g=G)),
    dict_child_of(DFI,G,DG),
       % tab(8),println(trace_entering_fun(i=I,g=G,H)),
    dict_child_of(DG,Ref,J),
        % tab(10),println(trace_entering_ind(i=I,j=J,g=G,ref=Ref)),
    I=J
  ; dict_child_of(DFN,0,DF0),
      % tab(8),println(trace_entering_fun_any(F/N)),
    dict_child_of(DF0,Ref,K),
      % tab(10),println(trace_entering_any(F/N,ref=Ref)),
    K=0
  ).
          
% MAIN INDEXED DB API
  
idb_assert(Db,C, Ref):-
  to_clause(C,HB),
  new_iclause(Db,HB,Ref),
  idb_index(Db,HB,Ref),
  % println(trace_asserting(Ref,HB)),
  true.

idb_asserted(DbName,HB,Ref):-
  idb_get_index(DbName,HB,Ref),
  % println(trace_here(Ref:HB)),
  iclause_get(DbName,Ref,HB),
  % println(trace_asserted(Ref:HB)),
  true.

idb_retract(DbName,H):-
  HB=(H:-_),
  idb_get_index(DbName,HB,Ref),
  % println(trace_here(Ref:HB)),
  iclause_rm(DbName,Ref),
  % println(trace_removed(Ref:HB)),
  true.

idb_retractall(DbName,H):-
  idb_retract(DbName,H),
  fail.
idb_retractall(_DbName,_H).
  
% tests if a clause head maps to something in the indexed db
idb_is_dynamic(DbName,H):-
  nonvar(DbName),
  root_dict(RD),
  dict_get(RD,DbName,D),
  D\=='$null',
  functor(H,F,N),
  % dict_get(D,F,DF),DF\=='$null',dict_get(DF,N,DFN),
  dict_get(D,F/N,DFN),
  DFN\=='$null'.

% cleans up a given DbName (if known), or all of them (if var)
idb_clean(DbName):-
  % todo - visit recursively and free symtable obs
  root_dict(RD),
  iclauses_clean(DbName),
  dict_remove(RD,DbName).
  
% SIMPLER API ELEMENTS
idb_assert(Db,HB):-
  idb_assert(Db,HB, _Ref).

idb_asserted(DbName,HB):-
  idb_asserted(DbName,HB,_Ref).

idb_clause(DbName,H,B):-idb_asserted(DbName,(H:-B),_Ref).

% FILE RELATED API AND LISTINGS
idb_consult(File,Db):-
  foreach(
    clause_of(File,C),
    idb_consult_action(C,Db)
  ).

idb_consult_action(':-'(G),Db):-if(G=[F],idb_consult(F,Db),once(idb_call(Db,G))).
idb_consult_action(':-'(H,B),Db):-idb_assert(Db,':-'(H,B),_).

idb_reconsult(File,Db):-
  idb_clean(Db),
  idb_consult(File,Db).

idb_save(Db,F):-
  telling(X),
  tell(F),
  idb_listing(Db),
  told,
  tell(X).
  
idb_listing:-idb_listing(_).

idb_listing(Db):-
  idb_listing(Db,_).
  
idb_listing(DbName,F/N):-
  (HB=(H:-_),functor(H,F,N)->true;true),
  idb_asserted(DbName,HB,Ref),
  write_chars("/*"),write(Ref),write_chars("*/ "),
  pp_clause(HB),
  fail.
idb_listing(_,_).  

idb_show_index:-idb_show_index(_).

idb_show_index(DbName):-
  println('INDEX'),
  HB=(H:-_),
  foreach(
    idb_show_index(DbName,F,N,I,_,G,Ref,HB),
    ( functor(H,F,N),HI=((F/N)->[I]->G),
      println(index(DbName,Ref):HI=>H)
    )
  ).

idb_show_index(DbName,F,N,I,J,G,Ref,HB):-
  Locked= -1,
  root_dict(RD),
  dict_child_of(RD,DbName,D),
  (functor(H,F,N)->HB=(H:-_);true),
  % tab(2),println(index_obs:[RD,D,DFN]),
  dict_child_of(D,F/N,DFN),
  %tab(4),println(index_obs:[RD,D,DFN]),
  dict_child_of(DFN,I,DFI),
  %tab(6),println(index_obs:[RD,D,DFN,DFI]),
  %tab(6),println(index_for(F/N-i(I),dfi(DFI))),
  ( I=:=0->dict_child_of(DFI,Ref,J)
  ; DFI\==Locked,dict_child_of(DFI,G,DG),
    dict_child_of(DG,Ref,J)
  ),
  iclause_get(DbName,Ref,HB).

idb_call(Db,Body):-idb_body(Db,Body).

idb_body(Db,Body):-var(Body),!,errmes(bad_metacall(Db),var(Body)).
idb_body(Db,Body) :-
	idb_body(Db,Body, AfterCut, HadCut),
	( HadCut = yes,
		!,
		idb_body(Db,AfterCut)
	;   HadCut = no
	).

idb_body(_Db,(!,AfterCut), AfterCut, yes) :- !.
idb_body(Db,(Goal,Body), AfterCut, HadCut) :- !,
	idb_goal(Db,Goal),
	idb_body(Db,Body, AfterCut, HadCut).
idb_body(_Db,!, true, yes).
idb_body(Db,(Disj1;_), AfterCut, HadCut) :-
	idb_body(Db,Disj1, AfterCut, HadCut).
idb_body(Db,(_;Disj2), AfterCut, HadCut) :- !,
	idb_body(Db,Disj2, AfterCut, HadCut).
idb_body(Db,Goal, true, no) :-
	idb_goal(Db,Goal).

idb_goal(_Db,Goal) :-
	is_compiled(Goal), % <--- check for a compiled predicate
	!,
	%println('calling compiled'(Goal)),
	Goal.
idb_goal(Db,Goal) :-
	idb_is_dynamic(Db,Goal),
  !,
	idb_clause(Db,Goal, Body),	% <--- assume anything else is interpreted
	idb_body(Db,Body, AfterCut, HadCut),
	(	HadCut = yes,
		!,
		idb_body(Db,AfterCut)
	;	HadCut = no
	).
idb_goal(Db,Undef):-
  idb_undefined(Db,Undef).
  
idb_undefined(Db,Undef):-
  errmes(undefined_predicate_in_call(Db),Undef).

% this DB

idb_is_dynamic(H):-
  this_db(Db),
  idb_is_dynamic(Db,H).

idb_assert(HB):-
  this_db(Db),
  to_clause(HB,C),
  idb_assert(Db,C,_Ref).

idb_retract(H):-
  this_db(Db),
  idb_retract(Db,H).
  
idb_clause(H,B):-
  this_db(Db),
  idb_clause(Db,H,B).
    
idb_call(G):-
  this_db(Db),
  idb_call(Db,G).

idb_consult(File):-
  this_db(Db),
  idb_consult(File,Db).
  
idb_reconsult(File):-
  this_db(Db),
  idb_reconsult(File,Db).

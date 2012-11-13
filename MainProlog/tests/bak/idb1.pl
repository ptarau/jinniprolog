% indexed dynamic database layer

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
  functor(A,G,_).

named_dict(DictName,Dict):-val('$dict',DictName,Dict),!.
named_dict(DictName,Dict):-new_dict(Dict),def('$dict',DictName,Dict).

root_dict(Dict):-named_dict('$root',Dict).

% if K is known, checks if it is a key, if not generates them all
dict_key(D,K):-nonvar(K),!,dict_get(D,K,V),V\=='$null'.
dict_key(D,K):-
  invoke_java_method(D,getKeys,Iter),
  iterator_element(Iter,K).
  
dict_child_of(ParentDict,DbName,Dict):-
  dict_key(ParentDict,DbName),
  % this will not insert a new child as 
  % dict_key only enumerates existing
  dict_get(ParentDict,DbName,Dict).  

% layer 1 - assumes first arg has gound top functor

idb_index(Db,HB,Ref):-
  ( nonvar(Db),HB=(H:-_),functor(H,F,_N)->
    true
  ; !,errmes(instatiation_error,adding_to(Db,Ref:HB))
  ),
  root_dict(RD),
  dict_ensure_child(RD,Db,D),
  dict_ensure_child(D,F,DF), % F->F/N?
  % I in 1..N or I=$any
  arg_path_of(H,I,G),
  dict_ensure_child(DF,I,DFI),
  dict_ensure_child(DFI,G,DG), % G->G/M
  dict_put(DG,Ref,I),
  fail.
idb_index(_Db,_HB,_Ref).

% index on retrieval
% TODO: use index_path_of((H:-_),F,N,I,G,M),
idb_get_index(DbName,(H:-_),Ref):-
  root_dict(RD),
  dict_child_of(RD,DbName,D),
    % tab(2),println(trace_entering_db(DbName,D)),
  (functor(H,F,_N)->true;true),
  dict_child_of(D,F,DF),
    % tab(4),println(trace_entering_pred(F=>DF)),
  idb_pick_indexed(DF,H, I,DFI,G),
    % tab(6),println(trace_entering_arg(i=I,g=G)),
  dict_child_of(DFI,G,DG),
    % tab(8),println(trace_entering_fun(i=I,g=G,H)),
  dict_child_of(DG,Ref,J),
    % tab(10),println(trace_entering_ind(i=I,j=J,g=G,ref=Ref)),
    I=J,  
  true.

% SEE WHICH I BRINGS KNOWN ARGS - PICK THE FIRST IF THERE, $any OTHERWISE
idb_pick_indexed(DF,H, I,DFI,G):-
  arg_path_of(H,I,G),
  dict_child_of(DF,I,DFI),
  !.
          
idb_assert(Db,HB, Ref):-
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

idb_listing:-foreach(idb_listing(_),true).
  
idb_listing(DbName):-
  idb_asserted(DbName,HB,Ref),
  println(Ref:HB),
  fail.
idb_listing(_).  

idb_show_index:-idb_show_index(_).

idb_show_index(DbName):-
  println('INDEX_OF'(DbName)),
  HB=(H:-_),
  foreach(
    idb_show_index(DbName,F,I,_,G,Ref,HB),
    ( functor(H,F,N),functor(HI,F,N),arg(I,HI,G),
      numbervars(HI,10,_),
      println(index(DbName,Ref):HI=>H)
    )
  ).

idb_show_index(DbName,F,I,J,G,Ref,HB):-
  root_dict(RD),
  dict_child_of(RD,DbName,D),
  dict_child_of(D,F,DF),
  dict_child_of(DF,I,DFI),
  dict_child_of(DFI,G,DG),
  dict_child_of(DG,Ref,J),
  iclause_get(DbName,Ref,HB).
  
idb_clean(DbName):-
  % todo - visit recursively and free symtable obs
  root_dict(RD),
  iclauses_clean(DbName),
  dict_remove(RD,DbName).
  
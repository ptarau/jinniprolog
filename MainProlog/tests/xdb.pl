% ARGS TO INDEX

arg_key_of(Db,H,[Db,F,N|Ks]):-
  functor(H,F,N),
  all_args(1,N,Is),
  gen_arg_key(H,Is,Ks).
arg_key_of(Db,_H,[Db]).
  
gen_arg_key(_,[],[]).
gen_arg_key(H,[I|Is],[I,G,M|Js]):-
  good_arg(H,I,G,M),
  gen_arg_key(H,Is,Js).
gen_arg_key(H,[_|Is],Js):-
  gen_arg_key(H,Is,Js).  

good_arg(H,I,G,M):-arg(I,H,A),functor(A,G,M),G\=='$float'.

all_args(N,N,[N]).
all_args(I,N,[I|Is]):-I<N,I1 is I+1,all_args(I1,N,Is).

constant_of(T,C,0):-atomic(T),!,C=T.
constant_of(T,CF,CN):-functor(T,F,N),pick_constant_of(T,F,N,CF,CN).

pick_constant_of(T,F,N,F,N):-indexable_constant(T,F,N).
pick_constant_of(T,_F,N,CF,CN):-for(I,1,N),arg(I,T,A),constant_of(A,CF,CN).

indexable_constant([_|_],_,_):-!,fail.
indexable_constant((_,_),_,_):-!,fail.
indexable_constant((_;_),_,_):-!,fail.
indexable_constant(_-_,_,_):-!,fail.
indexable_constant(T,_,_):-float(T),!,fail.
indexable_constant(_,_,_).


/*
xdb_assert(Db,HB,Ref):-
  HB=(H:-B),
  ground(Db),
  ground(H),
  !,
  arg_key_of(Db,H,K),
  new_idict(Db,HB,Ref),
*/  
  
% dict table

idicts_clean(Root,Db):-
  val(Root,Db,_),
  !,
  rm(Root,Db).
idicts_clean(_,_).  

new_idict(Root,Db,HB,Ref):-
  make_queue(Root,Db,Q),
  queue_add(Q,HB),
  queue_size(Q,L),
  Ref is L-1.
 
idict_get(Root,Db,Ref,HB):-
  val(Root,Db,Q),
  queue_at(Q,Ref,HB).

idict_rm(Root,Db,Ref):-
  val(Root,Db,Q),
  queue_rm(Q,Ref).
  
idict_all(Root,Db,Ref,HB):-
  idict_iterate(Root,Db,Q,Ref),
  queue_at(Q,Ref,HB).

idict_iterate(Root,Db,Q,Ref):-
  val(Root,Db,Q),
  queue_size(Q,L),
  Last is L-1,
  for(Ref,0,Last).
    
idicts_show(Root,Db):-
  idict_all(Root,Db,_Ref,HB),
  pp_clause(HB),
  fail.
idicts_show(_,_). 

% clauses

iclauses_clean(Db):-idict_clean(iclause,Db).
new_iclause(Db,HB,Ref):-new_idict(iclause,Db,HB,Ref).
iclause_get(Db,Ref,HB):-idict_get(iclause,Db,Ref,HB).
iclause_rm(Db,Ref):-idict_rm(iclause,Db,Ref).
iclause_all(Db,Ref,HB):-idict_all(iclause,Db,Ref,HB).
iclauses_show(Db):-idict_show(iclause,Db).

% dicts

idicts_clean:-idict_clean(root,dict).
new_idict(Name,Ref):-new_idict(root,dict,Name,Ref).
idict_get(Ref,Name):-idict_get(root,dict,Ref,Name).
idict_rm(Ref):-idict_rm(root,dict,Ref).
idict_all(Ref,Name):-idict_all(root,dict,Ref,Name).
idicts_show:-idicts_show(root,dict).

idb1:-
  Root=dbs,
  Db=mydb,
  idicts_clean(Root,Db),
  new_idict(Root,Db,(a(m):-true),_Ref1),
  new_idict(Root,Db,(a(n):-true),Ref2),
  new_idict(Root,Db,(a(n):-X=u;X=w),_Ref3),
  idict_rm(Root,Db,Ref2),
  idicts_show(Root,Db).
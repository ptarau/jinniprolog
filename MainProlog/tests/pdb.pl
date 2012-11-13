% clause table

idicts_clean(Db):-
  idict_iterate(Db,Ref),
  idict_rm(Db,Ref),
  fail.
idicts_clean(Db):-  
  init_gensym(Db).

new_idict(Db,HB,Ref):-
  gensym_no(Db,Ref),
  def(Db,Ref,HB).
 
idict_get(Db,Ref,HB):-
  val(Db,Ref,HB).

idict_rm(Db,Ref):-
  val(Db,Ref,_),
  rm(Db,Ref).
  
idict_all(Db,Ref,HB):-
  idict_iterate(Db,Ref),
  val(Db,Ref,HB).

idict_iterate(Db,Ref):-
  val(gensym,Db,Last),
  for(Ref,1,Last).
    
idicts_show(Db):-
  idict_all(Db,_,HB),
  pp_clause(HB),
  fail.
idicts_show(_). 

itest:-
  new_idict(dict,a,I),
  new_idict(dict,b,J),
  idict_rm(dict,I),
  new_idict(dict,c,K),
  println(I+J+K),
  idicts_show(dict).
  

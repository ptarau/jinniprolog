:-['idb'].
:-[dynbm].
:-[hmap].
:-[gdb].

idb:-idb(4).

idb(N):-
  if(N<5,Print=1,Print=0), % 1 print, 0 don't
  Db=mydb,
  MaxMult=1,
  idb_clean(Db),
  gensym_init(db_ctr),
  gensym_init(db_acc1),
  gensym_init(db_acc2),
  ctime(T1),
  idb_create(Db,N,MaxMult,Print),
  if(Print>0,idb_listing,true),
  if(Print>0,idb_show_index,true),
  ctime(T2),
  TCreate is T2-T1,println(time(tcreate(TCreate))),
  idb_access1(Db,N,Print),
  ctime(T3),
  TAcc1 is T3-T2,println(time(tacc1(TAcc1))),
  idb_access2(Db,N,Print),
  ctime(T4),
  TAcc2 is T4-T3,println(time(tacc2(TAcc2))).
  
idb_create(Db,N,MaxMult,Print):-
  HB=(e(X,I,J,K,T):-c(M)),
  for(I,1,N),
    for(J,1,N),
      for(K,1,N),
        random(R),
        0 is R mod 3,random(RC), % maybe assert a few
        M is RC mod MaxMult,
        for(L,0,M),
          gensym_no(db_ctr,X),
          Z is R mod 10,
          (0=:=Z->T=_;T=z(Z,L)),
          idb_assert(Db,HB,Ref),
          Print>0,
          println(asserting(Ref/M,HB)),
  fail.
idb_create(Db,N,MaxMult,_):-
  gensym_no(db_ctr,Max0),Max is Max0-1,
  println(created_db(total=Max,Db,N,MaxMult)).

idb_access1(Db,N,Print):-
  val(gensym,db_ctr,Total),
  HB=(e(X,_,_,_,_):-_),
  println('ACCESS1'(total(Total))),
  println(hb_pattern=HB),
  for(_,1,N),
    for(_,1,N),
      for(_,1,N),
        random(R),0 is R mod 3, % maybe look-up
        random(Y),
        X is (Y mod Total)+1,
        % println(hb_instance=HB),
        idb_asserted(Db,HB,Ref),
        gensym_no(db_acc1,_),
          Print>0,
          println(asserted(Ref,HB)),
  fail.
idb_access1(Db,N,_):-
  gensym_no(db_acc1,Max0),Max is Max0-1,
  println(accessed_db(total=Max,Db,N)).

idb_access2(Db,N,Print):-
  HB=(e(_,I,J,K,_):-_),
  val(gensym,db_ctr,Total),
  println('ACCESS2'(total(Total))),
  % println(hb=HB),
  for(I,1,N),
    for(J,1,N),
      for(K,1,N),
        random(R),0 is R mod 3, % maybe look-up
        % tab(2),println(trace_entering_access2(R,I,J,K)=>HB),
        idb_asserted(Db,HB,Ref),
        % tab(2),println(trace_exiting__access2(R,I,J,K)=>HB),
        gensym_no(db_acc2,_),
          Print>0,
          println(asserted(Ref,HB)),
  fail.
idb_access2(Db,N,_):-
  gensym_no(db_acc2,Max0),Max is Max0-1,
  println(accessed_db(total=Max,Db,N)).
    
idb2:-
  Db=mydb,
  idb_clean(Db),
  idb_assert(Db,(a(m):-true),_),
  idb_assert(Db,(a(n):-true),_),
  idb_assert(Db,(a(n):-X=u;X=w),_),
  idb_assert(Db,(b(k):-a(_)),_),
  idb_assert(Db,(a(X):-X=u;X=w),_),
  idb_assert(Db,(b(l):-a(n)),_),
  idb_retractall(Db,a(X)),
  foreach(
    idb_asserted(_Db,HB,Ref),
    println(Ref=>HB)
  ),
  idb_retractall(Db,c(X)),
  idb_show_index,
  idb_listing.  

idb1:-
  Db=mydb,
  iclauses_clean(Db),
  new_iclause(Db,(a(m):-true),_Ref1),
  new_iclause(Db,(a(n):-true),Ref2),
  new_iclause(Db,(a(n):-X=u;X=w),_Ref3),
  iclause_rm(Db,Ref2),
  iclauses_show(Db).

dbug:-
  new_dict(D),
  dict_put(D,a,1),
  new_dict(DD),
  dict_put(D,b,DD),
  dict_put(D,b,0),
  delete_java_object(DD),
  dict_put(D,a,1),
  dict_key(D,K),
  println(K),
  fail.
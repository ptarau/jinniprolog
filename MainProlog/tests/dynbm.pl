tx:-tx(2000).

tx(N):-
 XN is 1000.000001*N,
 M is 20*N,D is 20.000001,
 db_clean(db),
 idb_clean(idb),
 hmaps_init,
   tnop(M), % to trigger native comp
 
 ctime(T0),
   tnop(M),
   
 ctime(T1),
   tdb(N),
 ctime(T2),
   tidb(N),
 ctime(T3),
 
   tj(M),
 ctime(T4),
   tp(M),
 ctime(T5),
   tf(M),
 ctime(T6),
   tq(M),
 ctime(T7),
   tgdb(N),
 ctime(T8),
 
 TNOP is (T1-T0)/D,
 TDB is (((T2-T1)/D)-TNOP),
 TIDB is (T3-T2)-TNOP,
 TJ is ((T4-T3)/D)-TNOP,
 TP is ((T5-T4)/D)-TNOP,
 TF is (2*(T6-T5)/D)-TNOP,
 TQ is ((T7-T6)/D)-TNOP,
 TGDB is (T8-T7)-TNOP,
 
 XNOP is XN/TNOP,
 XDB is XN/TDB,
 XIDB is XN/TIDB,
 XTJ is XN/TJ,
 XTP is XN/TP,
 XTF is XN/TF,
 XTQ is XN/TQ,
 XGDB is XN/TGDB,
 
 (N<6->
   db_listing(db),
   idb_listing(idb),
   idb_show_index(idb)
 ; true
 ),
 
 foreach(
   member(X,[
     nop(ms(TNOP),XNOP/s),
     db(ms(TDB),XDB/s),
     idb(ms(TIDB),XIDB/s),
     tj(ms(TJ),XTJ/s),
     tp(ms(TP),XTP/s),
     tf(ms(TF),XTF/s),
     tq(ms(TQ),XTQ/s),
     gdb(ms(TGDB),XGDB/s)
   ]),
  println(X)
 ) .
 
tnop(N):-
  for(_I,1,N),
  fail.
tnop(N):-
  for(_I,1,N),
  fail.
tnop(_).

tj(N):-
  new_dict(D),
  tj(D,N),
  delete_java_object(D).
 
tj(D,N):-
  HB=(a(I,X,X):-true),
  %HB=a,
  for(I,1,N),
  dict_put(D,I,HB),
  fail.
tj(D,N):-
  HB=(a(I,X,X):-true),
  %HB=a,
  for(I,1,N),
  dict_get(D,I,HB),
  fail.
tj(_,_).

tf(N):-
  %new_java_class('prolog.logic.Fun',C),
  %new_java_object(C,args(f,a,b),F),
  tf1(N).
  %delete_java_class(C,_),
  %delete_java_object(F,_).
 
tf1(N):-
  H=f(a,g(b),c,I),
  for(I,1,N),
  % arg_path_of(H,K,A),term_hash([K,A],_),
  term_hash(H,_),
  % println(F:X),
  fail.
tf1(_).
  
tp(N):-
  tp(tp,N).
    
tp(D,N):-
  HB=(a(I,X,X):-true),
  for(I,1,N),
  let(D,I,HB),
  fail.
tp(D,N):-
  HB=(a(I,X,X):-true),
  for(I,1,N),
  val(D,I,HB),
  fail.
tp(_,_).

tq(N):-
  queue_create(Q),
  tq(Q,N).
    
tq(Q,N):-
  for(_I,1,N),
  queue_push(Q,a),
  queue_pop(Q,_),
  fail.
tq(_,_).


th(N):-
  hmap_new(D),
  th(D,N),
  hmap_clean(D).
  
th(D,N):-
  HB=(a(I,X,X):-true),
  for(I,1,N),
    hmap_put(D,I,HB),
  fail.
th(D,N):-
  HB=(a(I,X,X):-true),
  for(I,1,N),
    hmap_get(D,I,HB),
  fail.
th(_,_).

tdb(N):-
  HB=(a(I,X,X):-true),
  for(I,1,N),
  db_assert(db,HB),
  fail.
tdb(N):-
  HB=(a(I,X,X):-true),
  HB=(H:-_),
  for(I,1,N),
  db_clause(db,H,_),
  fail.
tdb(_).

tidb(N):-
  HB=(a(I,X,X):-true),
  for(I,1,N),
  idb_assert(idb,HB),
  fail.
tidb(N):-
  HB=(a(I,X,X):-true),
  HB=(H:-_),
  for(I,1,N),
  idb_clause(idb,H,_),
  fail.
tidb(_).

tgdb(N):-
  new_graph(G),
  tgdb(G,N).
  
tgdb(G,N):-
  HB=(a(I,X,X):-true),
  HB=(H:-B),
  for(I,1,N),
  gassert(G,gdb,H,B),
  fail.
tgdb(G,N):-
  HB=(a(I,X,X):-true),
  HB=(H:-_),
  for(I,1,N),
  gclause(G,idb,H,_),
  fail.
tgdb(_,_).

% db_test

dtest:-dtest(20).

dtest(Times):-
  Q=mydb,
  ( for(I,1,Times),
      random(X),
      Selector is X mod 7,
      dtest1(Selector,I,Q),
    fail
  ; db_clause(Q,H,B),
    pp_clause((H:-B)),
    fail
  ; db_clean(Q),
    println(end)
  ).

dtest1(0,I,Q):-
  dshow(0,db_assert(Q,(a(I):-b1(I)))).
dtest1(1,I,Q):-
  J is I//2,
  dshow(1,db_asserta(Q,(a(I):-b2(J)))).
dtest1(2,I,Q):-
  J is I//4,
  dshow(2,db_assert(Q,(a(I):-b4(I,J)))).
dtest1(3,I,Q):-
  J is I//4,
  dshow(3,db_retract1(Q,a(J))).
dtest1(4,I,Q):-
  J is I//2,
  dshow(4,db_retractall(Q,a(J))).    
dtest1(5,_I,Q):-
  random(N),0=:=N mod 5,
  dshow(5,db_abolish(Q,a,1)).
dtest1(6,I,Q):-
  db_clause(Q,H,_B),
  dshow(I,db_retract(Q,H)).
      
dshow(I,G):-println(entering(I):G),G,println(exiting(I):G).

% queue test

qtest0:-
  queue_create(Q),
  queue_add(Q,a(1)),
  queue_add(Q,b(X,X)),
  queue_add(Q,c(X,f(X))),
  queue_rm(Q,7),
  queue_list(Q,Xs),
  queue_size(Q,L),
  foreach(for(_I,1,L),(queue_pop(Q,E),println(E))),
  println(L+Xs).
  
qtest:-qtest(20).

qtest(Times):-
  queue_create(Q),
  ( for(I,1,Times),
      random(X),
      Selector is X mod 5,
      qtest1(Selector,I,Q),
    fail
  ; queue_list(Q,Xs),
      nth_member(X,Xs,I),
      println(I-X),
    fail
  ; queue_destroy(Q),
    println(end)
  ).

qtest1(0,I,Q):-
  qshow(0,queue_add(Q,b(1,I,I))).
qtest1(1,I,Q):-
  J is I//2,
  qshow(1,queue_push(Q,b(2,I,J))).
qtest1(2,I,Q):-
  J is I//4,
  qshow(2,queue_add(Q,b(4,I,J))).
qtest1(3,I,Q):-
  J is I//4,
  qshow(3,queue_del1(Q,b(_,_,J))).
qtest1(4,I,Q):-
  J is I//4,
  qshow(4,queue_memb1(Q,b(_,_,J))).
  
qshow(I,G):-G,!,println(I:G).
qshow(I,G):-println(I:failing(G)),fail.

% hash test
   
hash_test:-hash_test(20).

hash_test(Times):-
  for(I,1,Times),
    random(X),
    Selector is X mod 4,
    hash_test1(Selector,I),
  fail.
hash_test(_):-
  hash_get(K,T),
  println(K-T),
  fail.
hash_test(_):-
  findall(K,get_hash_key(K),Ks),
  println(saving=Ks),
  hash_save('temp.pl',g),
  println(clearing),
  hash_clear,
  println(end).

hash_test1(0,I):-
  hash_show(0,hash_put(a(I),b1(I))).
hash_test1(1,I):-
  J is I//2,
  hash_show(1,hash_put(a(J),b2(I))).
hash_test1(2,I):-
  J is I//4,
  hash_show(2,hash_put(a(J),b4(I))).
hash_test1(3,I):-
  J is I//2,
  hash_show(3,hash_rm(a(J),_X)).  

hash_show(I,G):-G,!,println(I:G).
hash_show(I,G):-println(I:failing(G)),fail.

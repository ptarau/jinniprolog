xgo:-
  clearpp,
  new_and_hub(3,E),

  bg(prod(E)),
  sleep(5),
  bg(prod(E)),
  sleep(5),
  overprod(E),
  %bg(consall(E)),
  bg(cons(E)),
  bg(cons(E)),
  sleep(2),
  spp(at_end),
  showpp,
  %and_hub_stop(E),
  true.
 

cons(E):-
  spp(starting(cons)),
  and_hub_get(E,0,X),
  and_hub_get(E,1,Y),
  and_hub_get(E,2,Z),
  spp(got=X+Y+Z),
  object_to_string(E,S),
  spp(after_get=S).

prod(E):-
  and_hub_set(E,2,c),sleep(2),
  and_hub_set(E,1,b),sleep(2),
  and_hub_set(E,0,a),sleep(2),
  spp(done(set)).
  
overprod(E):-  
  and_hub_set(E,2,cc),sleep(2),  
  object_to_string(E,S),
  spp(after_extra_set=S).

consall(E):-
   and_hub_all(E,All),
   spp(all=All).

btest:-
  clearpp,
  new_barrier(4,run_task,B),
  bg(b(0,B)),
  bg(b(1,B)),
  bg(b(2,B)),
  bg(b(3,B)),
  sleep(20),
  showpp,
  object_to_string(B,S),
  println(S).

run_task:-spp(running_goal),sleep(2).
  
b(N,B):-
  spp(b(N)=sleeps(N)),
  sleep(N),
  spp(b(N)=arrives),
  barrier_arrive(B),
  spp(b(N)=proceeds),
  sleep(1),
  spp(b(N)=finished).



dtest:-
  N = 3,Max is N-1,
  new_and_hub(N,B),
  foreach(for(K,0,Max),bg(dstream(100,K,B))),
  for(I,1,10),
  println(getting=I),
  and_hub_all(B,Xs),
  sum(Xs,R),
  println(Xs=>R),
  fail.
  
   
dstream(Steps,K,B):-
  for(I,1,Steps),
   ranint(2000,T),
   X is K*I,
   sleep_ms(T),println(K=>I),and_hub_set(B,K,X),
  fail
; true.

ranint(Max,I):-random(R),I is R mod Max.

% tools
  
spp(X):-assert(printed(X)).

showpp:-clause(printed(X),_),println(X),fail.
showpp.

clearpp:-abolish(printed/1).


meta_and(A):-println(A),nl,sleep(1),fail.
meta_and(true):-!.
meta_and((A,B)):-!,
  new_and_hub(2,H),
  bg(and(meta_and(A),and_hub_set(0,A))),
  bg(and(meta_and(B),and_hub_set(1,B))),
  and_hub_all(H,[A,B]),
  and_hub_stop(H),
  true.
meta_and(A):-
  clause(A,B),
  ( B=(_,_) -> meta_and(B)
  ; topcall(B)
  ).
    
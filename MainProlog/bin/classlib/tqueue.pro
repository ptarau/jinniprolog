% amortized O(1) queues with reverse operation: reuses list,stack

:-[list].

tqueue:-
  new(list,T),
  tail<=T.

add(X):-
  tail=>T,
  T:push(X).

remove(X):-
  isEmpty,
  !,
  tail=>T, 
  cloneFrom(T),
  reverse,
  pop(X),
  T:clear.
remove(X):-
  pop(X).

show:-
  handle=>Hs,
  tail=>T,
  T:getElements(Ts),
  println(Hs-Ts).

example:-
  add(a),
  add(b),
  remove(X),
  add(c),
  show,
  remove(Y),
  add(d),
  remove(Z),
  remove(U),
  println([X,Y,Z,U]),
  show.

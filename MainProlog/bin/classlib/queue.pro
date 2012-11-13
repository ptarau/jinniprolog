% amortized O(1) queues with reverse operation: reuses list,stack

:-[list].

queue:-
  % tail provided by This
  new(list,H),
  head<=H.

add(X):-
  push(X).

remove(X):-
  head=>H,
  H:isEmpty,
  !,
  reverse,
  pop(X),
  this(This),
  H:cloneFrom(This),
  clear.
remove(X):-
  head=>H,
  H:pop(X).

getElements(Es):-
  head=>H,
  handle=>Ts,
  H:getElements(Hs),
  reverse(Ts,Rs),
  det_append(Hs,Rs,Es).
  
show:-
  head=>H,
  handle=>Ts,
  H:getElements(Hs),
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

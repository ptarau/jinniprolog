% amortized O(1) queues with reverse operation
% self contained implementation

aqueue:-
  head<=[],
  tail<=[].

add(X):-
  tail=>Xs,
  tail<=[X|Xs].

push(X):-
  head=>Xs,
  head<=[X|Xs].

remove(X):-
  head=>[],
  !,
  tail=>Xs,
  reverse(Xs,[X|Rs]),
  head<=Rs,
  tail<=[].
remove(X):-
  head=>[X|Xs],
  head<=Xs.

show:-
  head=>Hs,
  tail=>Ts,
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

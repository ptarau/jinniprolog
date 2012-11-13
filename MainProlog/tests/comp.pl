go(X):-
  retractall(a(_)),
  assert(a(1)),
  assert(a(2)),
  c(X).

c(X):-a(X).


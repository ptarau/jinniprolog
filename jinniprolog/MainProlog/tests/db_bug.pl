bug:-
  db_assert(a,b),
  db_assert(b,a).


nobug1:-
  db_assert(a,(f(1):-g(2))),
  db_clause(a,X,B),println(X+B),fail.

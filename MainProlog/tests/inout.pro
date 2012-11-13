go:-go(10000).

go(N):-
  for(_,1,N),
  out(a),
  in(a),
  fail.
go(_):-
  println(end).
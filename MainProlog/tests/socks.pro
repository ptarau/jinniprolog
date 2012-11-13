% on the server side do ?-run_server.

go:-go(10000).

go(N):-
  ctime(T1),
  remote_run(println(times(N))),
  loop(N),
  ctime(T2),
  '-'(T2,T1,T),
  '*'(N,1000.0,P),
  '/'(P,T,S),
  println([time(T),calls_per_sec(S)]).

loop(N):-
  for(I,1,N),
   remote_run(eq(_,I)),
  fail.
loop(_).

  

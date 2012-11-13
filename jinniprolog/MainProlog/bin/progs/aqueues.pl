% amortized O(1) queues with reverse operation

create_queue([]-[]).

queue_add(X,Front-Back,Front-[X|Back]).

queue_push(X,Front-Back,[X|Front]-Back).

queue_pop(X,[]-Back,Front-[]):- !,reverse(Back,[X|Front]).
queue_pop(X,[X|Front]-Back,Front-Back).

clear_queue(_,NewQueue):-create_queue(NewQueue).

go:-
  create_queue(Q0),
  queue_add(a,Q0,Q1),
  queue_add(b,Q1,Q2),
  queue_pop(X,Q2,Q3),
  queue_add(c,Q3,Q4),
  queue_push(d,Q4,Q5),
  queue_pop(Y,Q5,Q6),
  queue_pop(Z,Q6,Q7),
  queue_pop(U,Q7,Q8),
  foreach(member(Q,[Q0,Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8]),println(Q)),
  println([X,Y,Z,U]).


new_queue([]/[]).

queue_add(X,Hs/Ts,Hs/[X|Ts]).

queue_remove(X,[]/Ts,NewHs/[]):-reverse(Ts,[X|NewHs]).
queue_remove(X,[X|Hs]/Ts,Hs/Ts).

go:-
  new_queue(Q0),
  queue_add(a,Q0,Q1),
  queue_add(b,Q1,Q2),
  queue_add(c,Q2,Q3),
  queue_remove(X,Q3,Q4),
  write(element=X),nl,
  write(queue=Q4),nl.
  
% queues - primitive

enq(O,S,X):-xenq(O,S,X).
deq(O,S,X):-xdeq(O,S,X).
qpush(O,S,X):-xqpush(O,S,X).
queue(O,S,Xs):-xqueue(O,S,Xs).
qmembc(O,S,C):-xqmembc(O,S,C).
qrm(O,S):-xqrm(O,S).
qdel(O,S,C):-xqdel(O,S,C).

% stacks - primitive based on queues

push(O,S,X):-xqpush(O,S,X).
pop(O,S,X):-xdeq(O,S,X).
stack(O,S,Xs):-xqueue(O,S,Xs).
smemb(O,S,X):-xqmembc(O,S,X).
srm(O,S):-xqrm(O,S).
sdel(O,S,X):-xqdel(O,S,X).

/* queues */
  
xenq(Db,Key,X):-make_queue(Db,Key,Q),queue_add(Q,X).
  
xdeq(Db,Key,X):-val(Db,Key,Q),queue_pop(Q,X).
     
xqpush(Db,Key,X):-make_queue(Db,Key,Q),queue_push(Q,X).

xqmembc(Db,Key,C):-val(Db,Key,Q),queue_list(Q,Cs),member(C,Cs).

% xqmembc(Db,Key,C):-val(Db,Key,Q),queue_size(Q,L),M is L-1,for(Ref,0,M),queue_at(Q,Ref,C).
  
xqueue(Db,Key,Cs):-val(Db,Key,Q),queue_list(Q,Cs).

xqrm(Db,Key):-val(Db,Key,Q),rm(Db,Key),queue_destroy(Q).

xqdel(Db,Key,O):-val(Db,Key,Q),queue_del1(Q,O).

queue_memb1(Q,O):-
  queue_op(Q,1,0,_),
  repeat,
    queue_op(Q,2,0,R),
    member(R,['$null',O]),
  !,
  R\=='$null'.

queue_del1(Q,O):-queue_memb1(Q,O),queue_op(Q,3,0,_).

queue_at(Q,I,O):-queue_op(Q,0,I,O),O\=='$null'.
queue_rm(Q,I):-queue_update_at(Q,I,'$null').



make_stack(Db,Key,Q):-make_queue(Db,Key,Q).

make_queue(Db,Key,Q):-val(Db,Key,JData),!,Q=JData.
make_queue(Db,Key,Q):-queue_create(Q),def(Db,Key,Q).
          

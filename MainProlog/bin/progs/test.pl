tqenq(Tb,X):-tb_call(Tb,enq(X),the(yes)).
tqdeq(Tb,X):-tb_call(Tb,deq,the(yes(X))).
tqpush(Tb,X):-tb_call(Tb,push(X),the(yes)).
tqueue(Tb,Xs):-findall(X,tqmemb_element(Tb,X),Xs).
tqmemb(Tb,X):-tqueue(Tb,Xs),member(X,Xs).
tqdel(Tb,X):-tb_call(Tb,qdel(X),the(yes(X))).
tqrm(Tb):-stop(Tb).
tqstat(Tb):-tb_call(Tb,qcall(statistics),_).

/* term base queue operations */

% creating a queue engine

tb_new_queue(Tb):-new_engine(done,tb_queue_server,Tb).

tb_queue_server:-tb_queue_server(Xs,Xs).
tb_queue_server(Hs1,Ts1):-from_engine(Q),tb_op(Q,Hs1,Ts1).

tb_op(enq(X),Xs,[X|Ys]):-return(yes),tb_queue_server(Xs,Ys).
tb_op(deq,Xs,Ys):-tb_op_qdeq(Xs,Ys).
tb_op(push(X),Xs,Ys):-return(yes),tb_queue_server([X|Xs],Ys).
%tb_op(queue,Xs,Ys):-tb_op_queue(Xs,Ys).
tb_op(qmemb(X),Xs,Ys):-tb_op_qmemb(X,Xs,Ys).
tb_op(qdel(X),Xs,Ys):-tb_op_qdel(X,Xs,Ys).
tb_op(qcall(Goal),Xs,Ys):-if(Goal,true,true),return(Goal),tb_queue_server(Xs,Ys).

%tb_op_queue(Xs,[]):-return(Xs),fail.
%tb_op_queue(Xs,Ys):-tb_queue_server(Xs,Ys).

tb_op_qmemb(X,Xs,[]):-R=element(X),member(X,Xs),return(R),fail.
tb_op_qmemb(_,Xs,Ys):-return(no_more),tb_queue_server(Xs,Ys).

tb_op_qdeq(Xs,Ys):-nonvar(Xs),Xs=[X|NewXs],!,return(yes(X)),tb_queue_server(NewXs,Ys).
tb_op_qdeq(Xs,Ys):-return(no),tb_queue_server(Xs,Ys).

tb_op_qdel(X,Xs,Ys):-nonvar_select(X,Xs,NewXs),!,return(yes(X)),tb_queue_server(NewXs,Ys).
tb_op_qdel(_,Xs,Ys):-return(no),tb_queue_server(Xs,Ys).

tb_qrm(Tb):-stop(Tb).

tb_call(Tb,Op,R):-to_engine(Tb,Op),get(Tb,R).

tqmemb_element(Tb,X):-tb_call(Tb,qmemb(X),the(A)),tqmemb_more(A,Tb,X).

tqmemb_more(no_more,_,_):-!,fail.
tqmemb_more(element(X),_,X).
tqmemb_more(element(_),Tb,X):-get(Tb,the(A)),tqmemb_more(A,Tb,X).

go:-
  tb_new_queue(Tb),
  tqenq(Tb,a(X,X)),
  tqenq(Tb,b(88)),
  tqenq(Tb,b(99)),
  tqpush(Tb,77),
  tqpush(Tb,b(66)),
  tqpush(Tb,55),
  tqenq(Tb,100.5),
  tqueue(Tb,Xs),
  println(Xs),
  tqdel(Tb,b(R)),
  println(del=b(R)),
  tqdeq(Tb,D),
  println(deq=D),
  tqueue(Tb,Ys),
  println(Ys),
  tqstat(Tb).


t1:-
  tb_new_queue(Tb),
  tb_call(Tb,enq(a(X,X)),AE),
  println(AE),
  tb_call(Tb,enq(b(99)),AE1),
  println(AE1),
  tb_call(Tb,push(77),AE),
  println(AE),
  tb_call(Tb,queue,Xs),
  println(Xs),
  tb_call(Tb,qdel(_),R),
  println(R),
  tb_call(Tb,deq,D),
  println(D),
  tb_call(Tb,statistics,_).
 
bm:-
  bm(10000).
  
bm(N):-
  time_goal(bm1(N)),
  time_goal(bm2(N)),
  time_goal(bm3(N)).

bm1(N):-
  (val(b,b,_)->rm(b,b);true),
  for(I,0,N),
  qpush(b,b,b(I)),
  enq(b,b,b(I)),
  deq(b,b,_),
  deq(b,b,_),
  I=N.
    
bm2(N):-
  tb_new_queue(Tb),
  for(I,0,N),
  tqpush(Tb,b(I)),
  tqenq(Tb,b(I)),
  tqdeq(Tb,_),
  % tqdeq(Tb,_),
  I=N,
  tqueue(Tb,Xs),
  length(Xs,L),
  tqstat(Tb),
  println(length=L),
  tqrm(Tb).
  
bm3(N):-
  rm_external(a,a),
  for(I,0,N),
  xqpush(a,a,a(I)),
  xenq(a,a,a(I)),
  xdeq(a,a,_),
  % xdeq(a,a,_),
  I=N,
  xqueue(a,a,Xs),
  length(Xs,L),
  stat,
  println(length=L).
  
t2:-
  new_java_object('java.util.Vector',V),
  invoke_java_method(V,
     addElement('$encoded'(f(X,X))),_),
  invoke_java_method(V,
     addElement('un_encoded'(f(X,X))),_),
     invoke_java_method(V,elementAt(0),E),
     invoke_java_method(V,elementAt(1),U),
     println(E+U).
  
t3:-
   S='f(X,[1,2|Ys],s(3.99,_,X),Ys)',
   sread_term(S,F),atom_codes(S,Cs),length(Cs,L),
   T='$encoded'(F),
   this_class_object(P),
   call_java_class_method(
      'prolog.logic.PortableTerm',
      test(T,P),
      R),
   println(L+R).
      
    
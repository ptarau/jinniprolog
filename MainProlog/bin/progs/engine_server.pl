/*
  Abstract queue API using engines as server agents
*/

% creates a new engine server providing queue operations
engine_server_new_queue(Tb):-new_engine(done,engine_server_queue_server,Tb).
% adds an element to the end of the queue
engine_add_element(Tb,X):-engine_server_call(Tb,add_element(X),the(yes)).
% removes an element from the front of the queue
engine_remove_element(Tb,X):-engine_server_call(Tb,remove_element,the(yes(X))).
% adds an element to the front of the queue
engine_push_element(Tb,X):-engine_server_call(Tb,push_element(X),the(yes)).
% returns a copy of the queue
engine_queue(Tb,Xs):-engine_server_call(Tb,queue,the(Xs-[])).
% delete an element of the queue
engine_delete_element(Tb,X):-engine_server_call(Tb,delete_element(X),the(yes(X))).
% prints out statistics of the engine server
engine_statistics(Tb):-engine_server_call(Tb,invoke_server_task(statistics),_).
% stops and reclains resources of the engine server
engine_server_stop(Tb):-stop(Tb).

/*
  Helper predicates/Implementation
*/
engine_server_queue_server:-
  engine_server_queue_server(Xs,Xs).
    
engine_server_queue_server(Hs1,Ts1):-
  from_engine(Q),
  engine_server_task(Q,Hs1,Ts1,Hs2,Ts2,A),
  return(A),
  engine_server_queue_server(Hs2,Ts2).

engine_server_task(add_element(X),Xs,[X|Ys],Xs,Ys,yes).
engine_server_task(remove_element,Xs,Ys,NewXs,Ys,X):-engine_server_task_remove(Xs,NewXs,X).
engine_server_task(push_element(X),Xs,Ys,[X|Xs],Ys,yes).
engine_server_task(queue,Xs,Ys,Xs,Ys,Xs-Ys).
engine_server_task(delete_element(X),Xs,Ys,NewXs,Ys,YesNo):-engine_server_task_delete(X,Xs,NewXs,YesNo).
engine_server_task(invoke_server_task(Goal),Xs,Ys,Xs,Ys,Goal):-if(Goal,true,true).

engine_server_task_remove(Xs,NewXs,YesNo):-nonvar(Xs),Xs=[X|NewXs],!,YesNo=yes(X).
engine_server_task_remove(Xs,Xs,no).

engine_server_task_delete(X,Xs,NewXs,YesNo):-nonvar_select(X,Xs,NewXs),!,YesNo=yes(X).
engine_server_task_delete(_,Xs,Xs,no).

engine_server_task_stop(Tb):-stop(Tb).

engine_server_call(Tb,Op,R):-to_engine(Tb,Op),get(Tb,R).


% fold operations

ifoldl(Engine,F,R1,R2):-get(Engine,X),ifoldl_cont(X,Engine,F,R1,R2).

ifoldl_cont(no,_Engine,_F,R,R).
ifoldl_cont(the(X),Engine,F,R1,R2):-call(F,R1,X,R),ifoldl(Engine,F,R,R2).

ifltest(R):-new_engine(I,for(I,1,5),E),ifoldl(E,-,0,R).
%R = -15

ifoldr(Engine,F,R1,R2):-get(Engine,X),ifoldr_cont(X,Engine,F,R1,R2).

ifoldr_cont(no,_Engine,_F,R,R).
ifoldr_cont(the(X),Engine,F,R1,R2):-ifoldr(Engine,F,R1,R),call(F,X,R,R2).

ireverse(Xs,Ys):-
  new_engine(X,member(X,Xs),E),
  ifoldl(E,rcons,[],Ys).  

%rcons(Y,X,[X|Y]).

ifrtest(R):-new_engine(I,for(I,1,5),E),ifoldr(E,-,0,R).
%R = 3

% yield mechanism

yield(X):-from_engine((X:-G)),G,return(X).


% injects a block G into the local context of
% the engine and receives a result after executing G

inject(E,G,X):-to_engine(E,G),get(E,X).

call_in(TopGoal,InnerGoal,Result):-
  new_engine(done,TopGoal,E),
  inject(E,InnerGoal,Result),
  inject(E,(done:-fail),_).

loop(X):-yield(X=>Y),loop(Y).

inc(X1,X2):-X2 is X1+1.

update_alist(X=A,As,Bs):-
  append(Before,[X=_|After],As)->
    append([X=A|Before],After,Bs)
  ; Bs=[X=A|As].

itest2:-
   call_in(
     loop([]),
     (
       (Xs=>Ys:-
         update_alist(x=a,Xs,Ys)
       )
     ),
     R
   ),
   println(R).
   
itest1:-
   call_in(
     loop(0),
     (
        (X=>Y:-
           inc(X,Y)
        )
     ),
     R
   ),
   println(R).
   

/*
  tests
*/
go:-
  engine_server_new_queue(Tb),
  engine_add_element(Tb,a(X,X)),
  engine_add_element(Tb,b(99)),
  engine_push_element(Tb,77),
  engine_queue(Tb,Xs),
  println(Xs),
  engine_delete_element(Tb,b(R)),
  println(del=R),
  engine_remove_element(Tb,D),
  println(remove_element=D),
  engine_queue(Tb,Ys),
  println(Ys),
  engine_statistics(Tb).


go1:-
  engine_server_new_queue(Tb),
  engine_server_call(Tb,add_element(a(X,X)),AE),
  println(AE),
  engine_server_call(Tb,add_element(b(99)),AE1),
  println(AE1),
  engine_server_call(Tb,push_element(77),AE),
  println(AE),
  engine_server_call(Tb,queue,Xs),
  println(Xs),
  engine_server_call(Tb,delete_element(_),R),
  println(R),
  engine_server_call(Tb,remove_element,D),
  println(D),
  engine_server_call(Tb,statistics,_).

/*
  benchmarks
*/
 
bm:-
  bm(20000).
  
bm(N):-
  time_goal(bm1(N)),
  time_goal(bm2(N)),
  time_goal(bm3(N)).

% prolog API queues
bm1(N):-
  ints(L,0,64),
  (val(b,b,_)->rm(b,b);true),
  for(I,0,N),
  xqpush(b,b,b(L)),
  xenq(b,b,b(I)),
  xdeq(b,b,_),
  xdeq(b,b,_),
  I=N.

% engine server queues    
bm2(N):-
  ints(L,0,64),
  engine_server_new_queue(Tb),
  for(I,0,N),
  engine_push_element(Tb,b(L)),
  engine_add_element(Tb,b(I)),
  engine_remove_element(Tb,_),
  engine_remove_element(Tb,_),
  I=N,
  % engine_statistics(Tb),
  engine_server_stop(Tb).

% prolog database queues  
bm3(N):-
  (val(a,a,_),rm(a,a)->true;true),
  for(I,0,N),
  xqpush(a,a,a(I)),
  xenq(a,a,a(I)),
  xdeq(a,a,_),
  xdeq(a,a,_),
  I=N.

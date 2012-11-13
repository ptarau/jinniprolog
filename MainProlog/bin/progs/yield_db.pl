/*
  Abstract dynamic database API using engines as server agents
*/

% creates a new engine server providing Prolog database operations
new_edb(Engine):-new_engine(done,queue_server,Engine).
% adds an element to the end of the database
edb_assertz(Engine,Clause):-ask_engine(Engine,add_element(Clause),the(yes)).
% adds an element to the front of the database
edb_asserta(Engine,Clause):-ask_engine(Engine,push_element(Clause),the(yes)).
% returns a instances of asserted clauses
edb_clause(Engine,Head,Body):-
  ask_engine(Engine,queue,the(Xs-[])),
  member((Head:-Body),Xs).
% delete an element of the database
edb_retract1(Engine,Head):-
  Clause=(Head:-_Body),
  ask_engine(Engine,delete_element(Clause),the(yes(Clause))).
% removes a database
edb_delete(Engine):-stop(Engine).

go:-
  new_edb(Db),
  edb_assertz(Db,(a(2):-true)),
  edb_asserta(Db,(a(1):-true)),
  edb_assertz(Db,(b(X):-a(X))),
  ( edb_clause(Db,H,B),
    portray_clause((H:-B)),
    fail
  ; true
  ).
  
/*
  Helper predicates/Implementation
*/
queue_server:-
  queue_server(Xs,Xs).
    
queue_server(Hs1,Ts1):-
  from_engine(Q),
  server_task(Q,Hs1,Ts1,Hs2,Ts2,A),
  return(A),
  queue_server(Hs2,Ts2).

server_task(add_element(X),Xs,[X|Ys],Xs,Ys,yes).
server_task(remove_element,Xs,Ys,NewXs,Ys,X):-
   server_task_remove(Xs,NewXs,X).
server_task(push_element(X),Xs,Ys,[X|Xs],Ys,yes).
server_task(queue,Xs,Ys,Xs,Ys,Xs-Ys).
server_task(delete_element(X),Xs,Ys,NewXs,Ys,YesNo):-
   server_task_delete(X,Xs,NewXs,YesNo).
server_task(invoke_server_task(Goal),Xs,Ys,Xs,Ys,Goal):-
   if(Goal,true,true).

server_task_remove(Xs,NewXs,YesNo):-
   nonvar(Xs),Xs=[X|NewXs],
   !,
   YesNo=yes(X).
server_task_remove(Xs,Xs,no).

server_task_delete(X,Xs,NewXs,YesNo):-
  select_nonvar(X,Xs,NewXs),
  !,
  YesNo=yes(X).
server_task_delete(_,Xs,Xs,no).

server_task_stop(E):-stop(E).

select_nonvar(X,XXs,Xs):-nonvar(XXs),XXs=[X|Xs].
select_nonvar(X,YXs,[Y|Ys]):-nonvar(YXs),YXs=[Y|Xs],select_nonvar(X,Xs,Ys).

ask_engine(Engine,Goal, X):-
  to_engine(Engine,Goal),
  get(Engine,X).

engine_yield(Answer):-
  from_engine((Answer:-Goal)),
  call(Goal),
  return(Answer).

loop(X):-engine_yield(X=>Y),loop(Y).

inc(X1,X2):-X2 is X1+1.
inc(X1,X2):-X2 is X1+1.

inc_test1(R1,R2):-
   new_engine(_,loop(0),E),
   ask_engine(E,(X=>Y:-inc(X,Y)),R1),
   ask_engine(E,(X=>Y:-inc(X,Y)),R2).

%%% 
sum_loop(S1):-
  engine_yield(S1=>S2),
  sum_loop(S2).

inc_test(R1,R2):-
   new_engine(_,loop(0),E),
   ask_engine(E,(S1=>S2:-S2 is S1+2),R1),
   ask_engine(E,(S1=>S2:-S2 is S1+5),R2).


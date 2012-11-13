xbm:-
  hbm,
  rpc_bm,
  sleep(2),
  tbm.

bm_goal(I,N,f(I)=f(N)).
%bm_goal(I,N,fail).
  
rbm:-
  rbm(1000).
  
rbm(N):-
  there,
  bm(N).

hbm:-
  hbm(5000).
  
hbm(N):-
  here,
  bm(N).

tbm:-
  tbm(1000).
  
tbm(N):-
  there,
  bg(run_server),
  bm(N),
  remote_run(stop_server).

bm:-
  bm(1000).
    
bm(N):-
  ctime(T1),
  bm0(N),
  ctime(T2),
  T is T2-T1,
  S is N/(0.001*T),
  println([time(steps(N),ms(T)),calls_per_sec(S)]).
  
bm0(N):-
  bm_goal(I,N,G),
  for(I,1,N),
  remote_run(G),
  fail.
bm0(_).

rpc_bm:-
  rpc_bm(5000).

rpc_bm(N):-
  ctime(T1),
  rpc_bm1(N),
  ctime(T2),
  T is T2-T1,
  S is N/(0.001*T),
  println([time(steps(N),ms(T)),calls_per_sec(S)]).

rpc_bm1(N):-
  bg(rpc_server),
  start_rpc,
  rpc_bm0(N),
  stop_rpc,
  bg(and(start_rpc,rpc(stop_server),stop_rpc)).
    
rpc_bm0(N):-
  bm_goal(I,N,G),
  for(I,1,N),
  rpc(G),
  fail.
rpc_bm0(_).
 
/* compile this file and type run_server on the server side */

one_task(Id,I,IdleTime):-
  println(task_for(Id,I)),
  sleep(IdleTime).
  
one_client(Id,Times,IdleTime):-
  for(I,1,Times),
    println(start_task(Id,I)),
    remote_run(one_task(Id,I,IdleTime)),
    println(end_task(Id,I)),
    sleep(IdleTime),
  fail.
one_client(Id,_,_):-
  remote_run(println(finished(Id))),
  shutdown.

/* run this, with various parameters on a W2000 or XP system 
   please make sure prolog.bat is in your path
*/
rruns(Clients,Times,IdleTime):-
  for(I,1,Clients),
    swrite(one_client(I,Times,IdleTime),Goal),
    make_cmd(['prolog.bat [netscale] "',Goal,'"'],Cmd),
    println(Cmd),
    bg(system(Cmd)),
    sleep(IdleTime),
  fail
; 
  println(started(all)).

/* example of small test which works fine */
go:-
  G=rruns(5,2,1),
  println(G),
  G.

test:-
  G=rruns(3,2,1),
  println(G),
  G.
  
% client side

rpc_test:-
  bg(rpc_server),
  sleep(3),
  rpc_test(10000).
  
rpc_test(N):-
  println(starting_rpc(times(N))),
  ctime(T1),
  start_rpc,
  for(I,1,N),
  rpc(X=I),
  X=N,
  stop_rpc,
  ctime(T2),
  T is T2-T1,
  L is 1000*(N/T),
  println([times(N),total_time(T),rpc_per_sec(L)]),
  M is 10*N,
  ints(Is,1,M),
  swrite(Is,S),
  ctime(T3),
  start_rpc,rpc(_=S),stop_rpc,
  ctime(T4),
  TX is T4-T3,
  println([large_data_size(M),total_time(TX)]),
  start_rpc,rpc(stop_server),stop_rpc.  

% validator

validate:-
  eng_test,
  bg_test,
  % println('STARTING RMI registry'),
  % rli_init, % starts rmiregistry
  rli_simple,
  rli_test,
  % socktest,so_test,
  println('ALL TESTS ENDING').
  
% engine test

eng_test:-eng_test(1000).

eng_test(N):-
  statistics,nl,
  for(_I,1,N),
    new_engine(ok,fail,E),
    get(E,_),
  fail.
eng_test(_):-
  statistics.
  
% on the server side do ?-run_server.

socktest:-socktest(10).

socktest(N):-
  H=localhost,
  Port=8765,
  bg(run_server(Port)),
  sleep(2),
  ctime(T1),
  remote_run(H,Port,println(times(N))),
  sockloop(H,Port,N),
  ctime(T2),
  T is T2-T1,
  println([calls(N),time_in_ms(T)]),
  println(stopping_server),
  remote_run(H,Port,stop_server).
  
sockloop(H,P,N):-
  for(I,1,N),
   remote_run(H,P,eq(_,I)),
  fail.
sockloop(_,_,_).


bg_test:-bg_test(8).

bg_test(N):-
  bg_tester(N,0).

bg_tester(0,K):-bg_leaf(K).
bg_tester(N,K):-
  N>0,
  N1 is N-1,
  K1 is 2*K,
  K2 is K1+1,
  bg(bg_tester(N1,K1),_),
  bg(bg_tester(N1,K2),_).
  
bg_leaf(K):-
  println(bg_leaf(K)).    


so_test:-so_test(3).

so_test1:-so_test1(3).

so_test2:-so_test2(3).

so_test(N):-
  so_test1(N),  
  so_test2(N).

so_test1(N):-
  println(part1),
  bg(run_server(9999)),
  so_starter(N,0),
  println(part1_all_launched).
  
so_test2(N):-
  println(part2),
  S is 2*N,sleep(S),
  so_stopper(N,0),
  sleep(S),
  remote_run(localhost,9999,println(will_stop_soon)),
  remote_run(localhost,9999,stop_server).

so_starter(0,K):-so_start(K).
so_starter(N,K):-
  N>0,
  N1 is N-1,
  K1 is 2*K,
  K2 is K1+1,
  so_starter(N1,K1),
  bg(so_starter(N1,K2)).

so_start(K):-
  P is 10000+K,
  remote_run(localhost,9999,println(started=P)),
  run_server(P).

so_stopper(0,K):-so_stop(K).
so_stopper(N,K):-
  N>0,
  N1 is N-1,
  K1 is 2*K,
  K2 is K1+1,
  so_stopper(N1,K1),
  bg(so_stopper(N1,K2)).

so_stop(K):-
  P is 10000+K,
  remote_run(localhost,9999,println(remote_stopped=P)),
  remote_run(localhost,P,stop_server).      
  

% RLI tests

rli_simple:-
  P=testport,
  rli_start_server(P),
  sleep(2),
  G=findall(I,for(I,1,10),Is),
  rli_call(P,G),
  println(Is),
  rli_stop_server(P).

rli_test:-rli_test(8).

rli_test1:-rli_test1(8).

rli_test2:-rli_test2(8).

rli_test(N):-
  rli_test1(N),  
  rli_test2(N).

rli_test1(N):-
  println(part1),
  rli_start_server(9999),
  rli_starter(N,0),
  println(part1_all_launched).
  
rli_test2(N):-
  println(part2),
  S is 2*N,sleep(S),
  rli_stopper(N,0),
  sleep(S),
  rli_call(9999,println(will_stop_soon)),
  rli_stop_server(9999).

rli_starter(0,K):-rli_start(K).
rli_starter(N,K):-
  N>0,
  N1 is N-1,
  K1 is 2*K,
  K2 is K1+1,
  rli_starter(N1,K1),
  rli_starter(N1,K2).

rli_start(K):-
  P is 10000+K,
  rli_call(9999,println(started=P)),
  rli_start_server(P).

rli_stopper(0,K):-rli_stop(K).
rli_stopper(N,K):-
  N>0,
  N1 is N-1,
  K1 is 2*K,
  K2 is K1+1,
  rli_stopper(N1,K1),
  bg(rli_stopper(N1,K2)).

rli_stop(K):-
  P is 10000+K,
  rli_call(9999,println(rli_stopped=P)),
  rli_stop_server(P).      
  
  
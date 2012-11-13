keep_alive_client:-
  default_host(H),
  keep_alive_client(H,7001).

keep_alive_client(P):-keep_alive_client(localhost,P).

keep_alive_client(H,P):-
   keep_alive_client(H,P,none).

keep_alive_client(H,P,W):-
  that_host<=H,
  that_port<=P,
  password<=W,
  that_service<=stopped.
  
start_client:-
  that_host=>H,
  that_port=>P,
  integer(P),P>0,
  !,
  new_quiet_client(H,P,Client),
  that_service<=Client.
start_client.

ping:-start_client,disconnect.

% same effect as kill_server
% but with a more accurate name and a different implementation
discontinue_new_services:-ask(stop_server).

kill_server:-kill_server(none).

kill_server(Pwd):-ask(server_stop(Pwd)),ask(true).

% runs and closes connection
run(Goal):-
  that_host=>H,
  that_port=>P,
  password=>W,
  remote_run(H,P,Goal,Goal,W,the(Goal)).

% ask service and keeps connection alive
ask(X,G,R):-
  if(that_service=>stopped,start_client,true),
  that_service=>Client,
  Client\==stopped,
  !,
  password=>W,
  % keeps connection alive
  ask_service(Client,X,G,W,R).
ask(X,G,R):-
  that_host=>H,
  that_port=>P,
  password=>W,
  \+((integer(P),P>0)),
  remote_run(H,P,X,G,W,R).
 
ask(G):-ask(G,G,the(G)).

disconnect:-
  that_service=>Client,
  ask(let(service_finished,yes)),
  disconnect(Client),
  that_service<=stopped.
  
test_client1:-
  test_client(1000,_,true).

test_client2:-
  test_client(100,I,println(I)).
  
test_client3:-
  test_client(100,_,findall(K,for(K,1,10),Ks)),
  println(Ks).
    
test_client(N,I,G):-
  println(test_client(N,G)),
  test_client(N,I,G,T),
  println(time=T).
      
test_client(N,I,G,T):-
  ctime(T1),
  for(I,1,N),
     ( ask(G)->true
       ; !,println(unexpected_failure(I,G))
     ),
  I=N,
  disconnect,
  ctime(T2),
  T is T2-T1.

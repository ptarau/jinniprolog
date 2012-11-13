rli_client:-default_port(P),rli_client(P).

rli_client(P):-rli_client(localhost,P).

rli_client(H,P):-
  that_host<=H,
  that_port<=P.

rli_client(H,P,W):-
  to_string(P,S),namecat(S,W,PW),
  rli_client(H,PW).
  
ping:-ask(true,_).

kill_server:-rli_stop_server. /* on default port! */

kill_server(Port):-rli_stop_server(Port).

kill_server(Host,Port):-rli_stop_server(Host,Port).
  
ask(X,G,R):-
 that_host=>Host,
 that_port=>Port,
 rli_call(Host,Port,X,G,R).

ask(G,R):-
 that_host=>Host,
 that_port=>Port,
 rli_call(Host,Port,G,R).
  
ask(Goal):-
  that_host=>Host,
  that_port=>Port,
  rli_call(Host,Port,Goal).
 
run(Goal):-ask(Goal).

disconnect.

test_client1:-
  test_client(1000,_,call(true)).

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
  ctime(T2),
  T is T2-T1.

%@@
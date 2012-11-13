% tester agent

% TESTS for registered_agent

% after starting a registry-agent in another window with
%?-new(registrar,A),A:register(SomeAgentUID).
%try:

% capabilities:

captest:-
  new(peer,P),P:(
    add_capabilities(println,[cool,output]),
    add_capabilities(for,[cool,iterator]),
    list_capabilities,
    foreach(
      [cool,iterator]@(I,1,3),
      [output,cool]@(hello=I)
    )
  ).

utest2:-
  new(agent(joe),J),
  new(agent(bill),B),
  println(created(J+B)),
  B:(uid=>U),println(U),
  B:ask_agent(joe,println(hello)).

utest1(N):-
  for(I,1,N),symcat(a,I,A),
  get_uid(A,UID),
  (N<10->println(UID);true),
  fail.
utest1(N):-println(end(N,uids)).

% TESTS for peer.pro: advertize/publish/subscribe

test_publisher:-
  new(peer,P),
  P:advertize(cool_content),
  Content=mycontent(I),
  (for(I,1,50),
    sleep(3),
    P:println(publishing=Content),
    P:publish(cool_content,Content),
    fail
  ; println(end_publishing)
  ).

test_subscriber:-
  new(peer,S),
  S:wait_for_advertizer(cool_content,PUID),
  S:subscribe(PUID,cool_content).
  
peertest:-
  peerdemo,
  peer3d.

rentest:-
  peerdemo,
  renpeer.

montest:-
  peerdemo,
  monpeer.
    
peerdemo:-
  new(peer,P),P:uid=>U,println(uid(p)=U),
  new(peer,Q),Q:uid=>V,println(uid(q)=V),
  Q:new_named_hub(myhub),
  Q:bg((named_hub_get(myhub,X),println(got=X))),
  %println('0_sec'),sleep(10),println('10_sec'),
  bg(P:named_hub_set(myhub,hello)),
  bg(launch_agents(10)).
 
launch_agents(N):-
  for(I,1,N),
    sleep(1),
    new(peer,R),
    % bg(
     R:
       (sleep(1),
        uid=>V,
        println(hello_from_agent(I):V)
       )
    % )
  ,fail
; println(launched(N,agents)).
  
peer3d:-  
  sleep(5),
  new(peer,M),
  M:ask(show3d).
  
renpeer:-
  sleep(5),
  new(peer,M),
  M:ask(renshow).

monpeer:-
  sleep(5),
  new(peer,M),
  M:ask(show_in_window).
      
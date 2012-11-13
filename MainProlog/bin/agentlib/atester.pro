:-[agent].

% tester agent

/*
  Self centered test agent: its client component
  talks to its server component on a local port.
*/

atester:-
  agent(simple_behavior,2000,localhost,2000,'').

simple_behavior:-simple_behavior(5).
      
simple_behavior(N):-
  toString(S),
  for(I,1,N),
  sleep(1),
  ask(println(message(I,from(default_agent,S)))),
  I=N,  
  println(halting_server),
  agent_stop.
  
atest:-
  broker:start,
  prolog_agent_start(5555),
  new(gui_agent('ask_agent(5555,println(hi))',joe),_A),
  sleep(2),
  prolog_ask_agent(joe,println('hi Joe')).
  
 
  
  
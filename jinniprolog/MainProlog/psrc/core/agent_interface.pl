/*
  Interface agent for classic Prolog programs not using
  classes from agentlib and classlib. The agent simply
  acts like a passive server that executes remote calls
  on predicates in the code space in which it is included
  as a header and as a simple client that asks other agents
  for services.
*/

/* 
  classic Prolog equivalent of 
   new(prolog_agent,P),P:serve.
*/
prolog_agent_start(Port):-
  Goal=(server_port<=Port),
  rli_start_server(Port),
  rli_call(Port,Goal),
  call(Goal).

prolog_agent_stop:-
  server_port=>Port,
  rli_stop_server(Port).

/* this also works with object(..) ids 
   however, object handles that are returned might be 
   different even if structrurally the same 
   therefore some of the simpler forms might fail
*/

prolog_agent_ask(H,P,X,G,R):-rli_call(H,P,X,G,R).

prolog_agent_ask(H,P,G,R):-rli_call(H,P,G,R).

prolog_agent_ask(H,P,G):-rli_call(H,P,G).

prolog_agent_ask(P,G):-prolog_agent_ask(localhost,P,G).

/*
  Broker agent: facilitates information sharing
  between local agents.  
*/

:-[agent].

broker:-start.

start:-
  % start_registry,
  sleep(2),
  default_broker(_H,P,_W),
  % agent(println(broker_started_on(H,P)),P,H,P,W),
  agent(P),
  sleep(2).


%@@

/*
  Local Agent - reacheable only on fictious internal ports.
*/

:-[agent].

/*
  Self centered test agent: its client component
  talks to its server component on an internal port.
*/

agent(Behavior,InPort,OutPort):-
  agent(Behavior,InPort,localhost,OutPort,none).

start:-
  agent(behave,this_port,that_port).

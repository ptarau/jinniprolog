/*
  Basic Agent Class. Encapsulates P2P abilities
  (client + server) and goal oriented behavor.
  
*/

:-[prolog_object].
:-[client].
:-[server].
:-[messaging].

default_broker(localhost,broker,'').

agent:-default_server_port(P),agent(P).

agent(PortName):-agent(true,PortName).

agent(Goal,PortName):-default_broker(H,P,W),agent(Goal,PortName,H,P,W).

/*
  - runs a goal in background
  - listens on a local port as a server
  - sets up client side communication to a broker agent
    listening on a given host and port
*/  
agent(Goal,MyPort,BrokerHost,BrokerPort,Password):-
  agent(Goal,MyPort,BrokerHost,BrokerPort,Password,_OutputArea).

agent(Goal,MyPort,BrokerHost,BrokerPort,Password,OutputArea):-
  server(MyPort,Password),
  start_services(
    and(
      client(BrokerHost,BrokerPort,Password),
      Goal
    ),
    OutputArea
  ).

start_services(Goal,OutputArea):-
  nonvar(OutputArea),
  !,
  serve(Goal,OutputArea).  
start_services(Goal,_):-
  serve(Goal).

ask_agent(H,P,X,G,R):-new(client(H,P),C),C:ask(X,G,R).

ask_agent(H,P,G,R):-new(client(H,P),C),C:ask(G,R).

ask_agent(H,P,G):-new(client(H,P),C),C:ask(G).

ask_agent(P,G):-ask_agent(localhost,P,G).

ask_broker(G):-ask(G). % default client already for this

kill_agent(PortName):-kill_server(PortName).

agent_stop:-server_port=>Self,kill_server(Self).

%@@
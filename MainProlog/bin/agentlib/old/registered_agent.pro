/*
  Registerd Agent Class. Obtains UIDs from a registrar manually, 
  with various get_uid and name2uid predicates.
*/

:-[agent].
:-[term_serializer]. % serialized term exchanges
% :-[sealed_term].   % serialize+secure exchanges
:-[publishing].
:-[coordination].
:-[capabilities].
:-[messaging].

default_registry(localhost,10000,none).

registered_agent:-registered_agent(_SyntheticName).

registered_agent(MyName):-registered_agent(MyName,true).

registered_agent(MyName,Goal):-registered_agent(MyName,localhost,Goal).

registered_agent(MyName,MyHost,Goal):-registered_agent(MyName,MyHost,none,Goal).

registered_agent(MyName,MyHost,MyPwd,Goal):-
  get_uid(MyName,MyHost,UID),
  uid<=UID,
  arg(3,UID,MyPort),
  %UID=..[_Id,MyName,MyHost,MyPort],
  server(MyPort,MyPwd),
  start_services(Goal).

ask_agent(Name,Goal):-ask_agent(localhost,Name,Goal).

ask_agent(Host,Name,Goal):-
  name2uids(Name,Host,R),
  R=[UID],
  arg(2,UID,Host),
  arg(3,UID,Port),
  ask_agent(Host,Port,none,Goal,Goal,the(Goal)).

ask_agent(H,P,W,X,G,R):-
  client(H,P,W),
  ask(X,G,R0),
  disconnect,
  R=R0.

ask_registrar(Goal):-
  default_registry(RegisterHost,RegisterPort,Pwd),
  ask_agent(RegisterHost,RegisterPort,Pwd,Goal,Goal,the(Goal)).
 
% not compatible with this protocol !!!
% DO NOT DO THIS: ask_agent(H,P,W,X,G,R):-remote_run(H,P,X,G,W,R).

autostart_registry(MyHost,Port,MyPwd):-
  MyHost==localhost,
  client(MyHost,Port,MyPwd),
  ( ping->true
  ; default_registrar(F),
    Registrar=..[F,Port,MyPwd],
    bg(new(Registrar,_A)),
    sleep(2)
  ).

% override this to use a different registrar
default_registrar(agent_space). % params: Port,MyPwd
  
get_uid(MyName,UID):-get_uid(MyName,localhost,UID).

get_uid(MyName,MyHost,UID):-
  default_registry(RegisterHost,RegisterPort,Pwd),
  get_uid(RegisterHost,RegisterPort,Pwd,MyName,MyHost,UID).
      
get_uid(RegisterHost,RegisterPort,Pwd, MyName,MyHost,UID):-
  G=ask_agent(RegisterHost,RegisterPort,Pwd, 
    UID,register(MyName,MyHost,UID),the(UID)),
  ( G->true
  ; autostart_registry(MyHost,RegisterPort,Pwd), % start local registry
    G % try again
  ).
    
name2uids(RegisterHost,RegisterPort,Pwd, MyName,MyHost,MyPort, UIDs):-
  ask_agent(RegisterHost,RegisterPort,Pwd,
    UIDs,find_uids(MyName,MyHost,MyPort,UIDs),the(UIDs)).
      
name2uids(MyName,MyHost,UIDs):-
  default_registry(RegisterHost,RegisterPort,Pwd),
  name2uids(RegisterHost,RegisterPort,Pwd, MyName,MyHost,_MyPort,UIDs).

name2uids(MyName,UIDs):-name2uids(MyName,_MyHost,UIDs).  



  

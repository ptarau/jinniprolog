% Remote Predicate Calls

% high throughput client+server with socket reuse reused

% server side

rpc_server:-
  default_this_port(P),
  default_password(W),
  rpc_server(P,W).

rpc_server(Port,Password):-
  default_timeout(T),
  rpc_server(Port,Password,T).

rpc_server(Port,Password,Timeout):-
  new_server(Port,Server),
  Server\=='$null',
  serve(Server,Password,Timeout).
    
serve(Server,Password,Timeout):-
  repeat,
    ( new_service(Server,Timeout,Service)->
      bg(service_loop(Service,Password)),
      % stat,nl,
      fail
    ; true
    ),
    !.

starting_service:-let(service_finished,no).
stop_service:-let(service_finished,yes).
service_stopped:-val(service_finished,yes).

service_loop(Service,Password):-
  starting_service,
  repeat,
    service_step(Service,Password),
  !,
  disconnect(Service).
  
service_step(_Service,_Password):-service_stopped,!.
service_step(Service,Password):-answer_one_query(Service,Password),!,fail.
service_step(_Service,_Password).
  
start_rpc:-
  default_host(H),
  default_port(P),
  default_password(W),
  start_rpc(H,P,W).
  
start_rpc(H,P,W):-
  new_client(H,P,Client),
  def(host,H),
  def(port,P),
  def(password,W),
  def(client,Client),
  !.
start_rpc(H,P,_W):-
  errmes(rpc_client_alredy_started,on(H,P)).
   
stop_rpc:-
  val(client,Client),
  val(password,W),
  !,
  rm(client),
  rm(host),
  rm(port),
  rm(password),
  stop_service(Client,W).
stop_rpc.

stop_service(Client,W):-
  'ask'(Client,_,stop_service,W,_),
  disconnect(Client).

get_client_password(C,W):-
  val(client,C),
  val(password,W).  

rpc(G):-rpc(G,G,the(G)).

rpc(X,G,R):-get_client_password(C,W),'ask'(C,X,G,W,R).

% see net.pl
ask(Client,X,G,W,R):-ask_service(Client,X,G,W,R).

% COMPATIBILITY PACKAGE

new_service(Server,_Timeout,Service):-new_service(Server,Service).

default_this_port(7001).
default_port(7001).
default_password(none).
default_timeout(0).
default_host(localhost).
default_this_host(localhost).


% special purpose client/servers

run_object_server:-run_object_server(6001).
run_object_server(Port):-run_object_server(Port,none).
run_object_server(Port,Pwd):-new(Prolog),Prolog:(pass_objects,run_server(Port,Pwd)).

remote_run_object(X,G,R):-remote_run_object(localhost,6001,X,G,R).

remote_run_object(H,P,X,G,R):-remote_run_object(H,P,X,G,none,R).

remote_run_object(H,P,X,G,W,R):-new(Prolog),Prolog:(pass_objects,remote_run(H,P,X,G,W,R)).

pass_objects:-encode0,decode0. %,enact0.

decode0:-clause(term_decoder(_,_),_),!.
decode0:-asserta(term_decoder(T,T)).

encode0:-clause(term_encoder(_,_),_),!.
encode0:-asserta(term_encoder(T,T)).

enact0:-clause(rpc_action_handler(_,_,_),_),!.
enact0:-asserta((rpc_action_handler(I,_P,O):-on_rpc(I,O))).

pass_strings:-
  retractall(term_decoder(_,_)),
  retractall(term_encoder(_,_)),
  retractall(rpc_action_handler(_,_,_)).

% on_rpc(I,O):-println(I),O=got(I).

/*
simple_console:-
  C=(simple_console_action(I,I):-true),C=(H:-B),
  (clause(H,B)->true;asserta(C)),
  new_console.
 
object_server_action(I,O):-
  atom_codes(I,ICs),
  chat_step(ICs,OCs),
  atom_codes(O,OCs).
*/

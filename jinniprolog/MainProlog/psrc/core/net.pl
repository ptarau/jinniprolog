%term_decoder(T,T).
%term_encoder(T,T).

default_http_port(8001).
default_http_root(('.')).

run_http_server:-
  default_http_port(Port),
  run_http_server(Port).
  
run_http_server(Port):-
  default_http_root(Root),
  run_http_server(Port,Root).

run_http_server(Port,Root):-
  println(starting_http_server_on(port(Port),www_root(Root))),
  new_java_class('prolog.core.HttpService',C),
  invoke_java_method(C,run_http_server(Port,Root),_).

% TCP-IP tunelling

% API

/* Creates a server side TCP-IP tunnel which maps ServerPort to LinkServerPort.
   When a client connects to LinkServerPort, it will be able to
   map all the services of a server it has access to (usually 
   behind a firewall or NAT) as if they were provided on ServerPort.
   This component is meant to be used on a machine visible on the net
   to which services behind firewalls can connect and become visible.
   The returned ServerTunnel can be used to stop the (background) threads
   created to support the tunnel.
*/  
server_tunnel(ServerPort,LinkServerPort,ServerTunnel):-
  new_java_class('prolog.core.NetTunnel',C),
  invoke_java_method(C,server(ServerPort,LinkServerPort),ServerTunnel).


/* Creates a client side TCP-IP tunnel which maps LinkHost, LinkPort,
   to LocalServer, LocalPort where a server (usually behind a firewall
   or NAT) listens. This client connects to LinkHost, LinkPort, and
   remaps the services at LocalServer, LocalPort as if they were provided
   on ServerPort on the LinkHost machine. This component is meant to be 
   used on a machine invisible on the net (behind firewalls or NAT).
   The returned ServerTunnel can be used to stop the (background) threads
   created to support the tunnel.
*/  
client_tunnel(LinkHost,LinkPort,LocalServer,LocalPort,ClientTunnel):-
  new_java_class('prolog.core.NetTunnel',C),
  invoke_java_method(C,client(LinkHost,LinkPort,LocalServer,LocalPort),ClientTunnel).

/*
   Stops the threads supporting a server or client side tunnel.
*/
stop_tunnel(Tunnel):-
  invoke_java_method(Tunnel,stop,_).



% SERVER SIDE TUNNEL allocation mechanism 

activate_server_tunnel(No):-
  no2ports(No,ServerPort,LinkPort),
  server_tunnel(ServerPort,LinkPort,ServerTunnel),
  bb_def(tunnel_handle,No,ServerTunnel).

free_tunnel(No):-
  bb_val(tunnel_handle,No,Tunnel),
  bb_rm(tunnel_handle,No),
  stop_tunnel(Tunnel). 
  
no2ports(No,ServerPort,LinkPort):-
  ServerPort is 10000+No,
  LinkPort is 20000+No.

% CLIENT SIDE TUNNEL mechanism     
  
% default tunnel server

tunnel_server(SN):-bb_val(tunnel,server,SN),!.
tunnel_server('logic.csci.unt.edu'). % default tunnel server

set_tunnel_server(SN):-bb_let(tunnel,server,SN).

% client side tunnel creation towards well known tunnel server
http_tunnel(TunnelNo):-tunnel_from(80,TunnelNo).

tunnel_from(FromLocalPort,TunnelNo):-
  tunnel_from(localhost,FromLocalPort,TunnelNo).
 
tunnel_from(FromLocalServer,FromLocalPort,TunnelNo):-
  no2ports(TunnelNo,ServerPort,LinkPort),
  tunnel_server(SN),
  client_tunnel(SN,LinkPort,FromLocalServer,FromLocalPort,_T),
  println('Tunnel created'(
    from_local_port(FromLocalServer),
    to(ServerPort)
  )).
  
/*
  Creates HTTP tunnels to well known tunnel server
*/  
jinni_tunnel:-
  tunnel_server(SN),
  default_port(P),
  remote_run(SN,P,new_tunnel(No)),
  jinni_http_tunnel(No),no2ports(No,HTTP_port,_),
  make_cmd(['http://',SN,':',HTTP_port],Link),
  println('HTTP tunnel created from current directory to:'),
  println(Link).
  
   
% start HTTP server and tunel to same port
jinni_http_tunnel(No):-
  no2ports(No,ServerPort,LinkPort),
  tunnel_server(LinkHost),
  HTTP_port is ServerPort, % local same as remote port no
  jinni_http_tunnel(LinkHost,LinkPort,HTTP_port,'.',_Tunnel).
       
jinni_http_tunnel(LinkHost,LinkPort,HTTP_port,Dir,Tunnel):-
  bg(run_http_server(HTTP_port,Dir)),
  client_tunnel(LinkHost,LinkPort,localhost,HTTP_port,Tunnel).
       
% client side RPCs

new_client(Host,Port,Client):-new_client(Host,Port,1,Client).

new_quiet_client(Host,Port,Client):-
  new_client(Host,Port,0,Client),
  invoke_java_method(Client,connect,TF),
  is_true(TF).

new_client(Host,Port,TryConnect,Client):-
  new_java_object('prolog.core.Transport'(Host,Port,TryConnect),Client).

disconnect(ServiceOrClient):-
  invoke_java_method(ServiceOrClient,disconnect,_),
  delete_java_object(ServiceOrClient).

ping(Host,Port):-new_quiet_client(Host,Port,Client),disconnect(Client).

discontinue(Service):-
  invoke_java_method(Service,discontinue,_).
  
ask_query_get_answer(Client,QW,A):-
  write_to(Client,QW),
  read_from(Client,A),
  A\=='$null'.

ask_service(Client,X,G,W,R):-
  encode_term(the(X,G,W),QW),
  ask_query_get_answer(Client,QW,A),
  decode_term(A,T),
  % delete_java_object(QW),
  % delete_java_object(A),
  !,
  R=T.
ask_service(_,_,_,_,no).
    
ask_server(H,P,X,G,W,R):-
  new_client(H,P,Client),      
  ask_service(Client,X,G,W,R),
  disconnect(Client).

remote_run(G):-remote_run(localhost,7001,G).
remote_run(H,P,G):-remote_run(H,P,G,G,the(G)).
remote_run(H,P,X,G,R):-remote_run(H,P,X,G,none,R).

remote_run(H,P,X,G,_W,R):-val(where,here),!,run_here(H,P,X,G,R).
% remote_run(_H,_P,X,G,_W,R):-val(where,bp),!,call_bp(X,G,R).
remote_run(H,P,X,G,W,R):-integer(P),P>0,!,ask_server(H,P,X,G,W,R).
remote_run(H,P,X,G,_W,R):-H==localhost,run_here(H,P,X,G,R).

here:-let(where,here).
there:-let(where,there).
where(X):-val(where,X).

run_here(H,P,X,G,R):-ensure_new_space(H,P,E),copy_term((X:-G),(NewX:-NewG)),E:topcall(once(NewG)),!,R=the(NewX).
run_here(_,_,_,_,no).

ensure_new_space(H,P,E):-val(H,P,X),!,X=E.
ensure_new_space(H,P,E):-new(prolog,E),let(H,P,E).

new_server(Port,Server):-
  new_java_object('prolog.core.Transport'(Port),Server).
   
new_service(Server,Service):-
  call_java_class_method('prolog.core.Transport',newService(Server),Service),Service\=='$null'->true
  ; delete_java_object(Server),throw(unable_to_start_service_on_given_port).

read_from(Service,Query):-
  invoke_java_method(Service,read_from,Query).

write_to(Service,Answer):-
  invoke_java_method(Service,write_to(Answer),_).

% server side RPC handling
   
run_server:-run_server(7001).

run_server(Port):-
  run_server(Port,none).
   
run_server(Port,Password):-
  integer(Port),Port>0,
  !,
  new_server(Port,Server),
  Server\=='$null',
  repeat,
    ( new_service(Server,Service),Service\=='$null'-> 
      true 
    ; !,
      fail
    ),
    bg(handle_service(Service,Password)),
  fail.
run_server(Port,Password):-
  println('Internal Server started on port'(Port)),
  in('$internal_server'(Port,Password)).

    
handle_service(Service,Password):-
  if(answer_one_query(Service,Password),true,true),
  disconnect(Service),
  fail.
 
answer_one_query(Service,Password):-
  % stat,nl,
  read_from(Service,QString),
  QString\=='$null',
  decode_term(QString,QTerm),
  % println(got=QTerm),
  react_to(QTerm,Password,ATerm),
  % stat,println('----'),nl,
  encode_term(ATerm,AString),
  write_to(Service,AString),
  % ttyprint([sent=AString,qterm=QTerm]),ttynl,
  (arg(2,QTerm,stop_server)->discontinue(Service);true),
  (atom(QString)->delete_java_object(QString);true),
  (atom(AString),AString\==no->delete_java_object(AString);true).

stop_server:- 
  %println(server_stopped),
  true.
  
% defines how RPC requests are handled

react_to(Term,Password,Answer):-
  call_ifdef(rpc_action_handler(Term,Password,Result),fail),
  !,
  Answer=Result.
react_to(Term,Password,Answer):-
  run_query_term(Term,Result,Password),
  !,
  Answer=the(Result).  
react_to(_,_,Answer):-
  Answer=no.

decode_term(String,Term):-call_ifdef(term_decoder(String,Term),sread_term(String,Term)).
encode_term(Term,String):-call_ifdef(term_encoder(Term,String),swrite(Term,String)).

run_query_term(the(Answer,Goal,CPassword),Answer,SPassword):-call_here(CPassword,SPassword,Goal).
run_query_term(run(CPassword,Answer,Goal),Answer,SPassword):-call_here(CPassword,SPassword,Goal). % BinProlog compatibility?

% catch_once avoids leaking the engine used to run the goal
call_here(CP,SP,G):-CP==SP,!,call_here(G).
call_here(CP,_,G):-println(warning(wrong_password_for_query(G),passwd(CP))).

call_here(G):-call_ifdef(rpc_goal_handler(G),catch_once(topcall(G),_any,fail)).

% for BinProlog compatibility

run(_,G,_):-println(unexpected_topcall(G)).

/* remote client and server shells */

shell_server:-shell_server(6001).

shell_server(Port):-
  new_java_object('prolog.core.ShellServer'(Port),S),
  invoke_java_method(S,run,void,_).

shell_client:-shell_client(localhost,6001).  

shell_client(Host,Port):-
  new_java_object('prolog.core.ShellClient'(Host,Port),C),
  invoke_java_method(C,run,void,_).  
  
new_uid(U):-call_java_class_method('prolog.logic.Tools',uid,U).



%%%%%%%%%%%%% - Lean Prolog communicator %%%%%%%%%%

new_agent:-newcall(new_agent).

new_agent(Name):-newcall(new_agent(Name)).
ask_agent(Name,Goal):-newcall(ask_agent(Name,Goal)).

newtop:-
  newcall(apply_do_after_load),
  newcall(toplevel(main)),!.
newtop.

newcall(G0):-
  %seeing(StdIn),telling(StdOut),
  current_engine_object(E),
  export_term(G0),
  % call LeanProlog
  call_java_class_method('prolog.core.LogicConnector',ask_lprolog(E),Ok),
  %see(StdIn),tell(StdOut),
  import_term(TheG),
  Ok=yes,
  %println(newcall(TheG,Ok)),
  TheG=the(G),
  G\==exception,
  G=G0.

import_term(G):-
  current_engine_object(M),
  invoke_java_method(M,importBundle(0),_),
  from_engine(G).
  
export_term(G):-
  current_engine(E),
  current_engine_object(M),
  to_engine(E,G),
  invoke_java_method(M,exportBundle(0),_).
  
s_server:-s_server(4444).

the(G):-G.

the(_,G):-G.

s_server(Port):-
  % xcall(73,i(Port),the(yes))
  call_java_class_method('prolog.core.Transport',server(Port),_). 

s_connection(Connection):-s_connection(localhost,4444,Connection).

s_connection(Host,Port,Connection):-
  %xcall(74,i(Host,Port),Connection)
  call_java_class_method('prolog.core.Transport',connection(Host,Port),Connection).

s_run_at(Connection,T):-s_run_at(Connection,T,R),maybe_the(R,T).

s_run_at(Connection,T0,R):-
  T=call(T0),
  current_engine_object(E),
  export_term(T),
  %xcall(75,i(E,Connection),_), % ASK_SERVER
  %println(exported(T)),
  call_java_class_method('prolog.core.Transport',ask_lserver(Connection,E),_Ok),
  %println(ok=Ok),
  import_term( R).

maybe_the(the(call(X)),R):-!,R=X.
maybe_the(call(X),R):-!,R=X.
maybe_the(X,X).

s_disconnect(C):-disconnect(C).

s_call(G):-
  %println('HERE'(G)),
  s_connection(C),
  s_run_at(C,G,R),
  s_disconnect( C),
  maybe_the(R,G).

s_client_test:-
  s_connection(C),
    s_run_at(C,println(hello)),
    s_run_at(C,println(bye)),
    s_run_at(C,findall(X,member(X,[a,b,c]),Xs)),
  s_disconnect(C),
  foreach(member(X,Xs),println(X)).

s_client_test(N,Len):-
  s_connection(C),
  time(lsock_test1(C,N,Len)),
  s_disconnect(C).


lsock_test1(C,N,Len):-
  numlist(1,Len,Xs),
  for(_,1,N),
    s_run_at(C,length(Xs,_)),
    %println(Xs+L),
  fail.
lsock_test1(_,N,Len):-
  println(done(N,len(Len))).  
  
  
    
  
% external call import/export API - see Start.java
% calling this only makes sense from external Java code

'$prolog_loop':-import_loop.
  
import_loop:-
  println('PrologStarted'),
  repeat,
    println(leanPrologStep),
    ( import_term(G), % gets goal given by askProlog(Interactor,Bundle)
      call(G) % calls goal deterministically
    ->R=the(G),Ok=1
    ; R=no,Ok=0
    ),
    export_term(R), % returns answer as a bundle of type Object[]
    return(Ok),      % the array Object[2]={refs,cells}
  fail.
  

agui:-call_java_class_method('agentgui.Main',startgui,_).
  
% end
  
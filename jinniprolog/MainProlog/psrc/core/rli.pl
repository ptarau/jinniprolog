
/*
rli.pl

R(emote) L(ogic) I(nvocation) API

=======================================================================
Make sure you start rmiregistry, preferably by typing its name into
a new command window - so that you can see it is there :-)

The basic API is quite simple:

% deprecated - it is automatic now
rli_init: to be run in a separate window - starts rmiregistry
          must be called before anything else is called in the API
          deprecated - this is now automatic
          
rli_start_server(Port): starts a rli server on a new Prolog thread

rli_start_server(Port,Writer): starts server with output directed to Writer

rli_stop_server(Host,Port): stops a (possibly remote) rli server

rli_call(Host,Port,Goal): calls a Prolog goal on a remote server 
                          and binds the variables in Goal
                          with the first answer computed remotely

rli_in(Host,Port,Term): waits for a term to be posted on server at Host,Port

rli_out(Host,Port,Term): posts a Term on server at Host,Port - to be consumed
                         by the first rli_in waiting for it
                         


Note that "ports" are simply "names" that are "virtualized" by RMI. They can 
be integers or prolog atoms i.e. 9999 or 'snowflake' or 'Mary' qualify.
Think about them as "passwords" a client needs to know to access a server.
===========================================================================
*/

% tool interface

invoke_rli_method(MethodAndArgs,Result):-
  new_java_class('rli.RLIAdaptor',C),
  functor(MethodAndArgs,F,_),
  invoke_java_method(C,C,F,MethodAndArgs,Result).
 
/* basic API */

/*
rli_activate:-call_java_class_method('java.lang.System',setProperty('java.rmi.server.codebase','file:/bin/prolog.jar'),_).

% obsolete
rli_init:-bg(system('cmd /c START rmiregistry')),rli_activate.

rli_init_unix:-
  println('remember to kill process rmiregistry if needed'),
  bg(system('rmiregistry')),
  rli_activate.
*/

rli_start_server(Port,Writer):-
  this_class_object(Prolog),
  invoke_rli_method(rli_start_server(Port,Prolog,Writer),InstanceId),
  set_instance_id(InstanceId).
  
rli_start_server(Port):-
  this_class_object(Prolog),
  invoke_rli_method(rli_start_server(Port,Prolog),InstanceId),
  set_instance_id(InstanceId).
  
rli_stop_server(Host,Port):-bg(and(sleep(1),invoke_rli_method(rli_stop_server(Host,Port),_))).

rli_call_nobind(Port,Goal):-rli_call_plain(localhost,Port,Goal,_Result).

rli_call(Host,Port,Goal):-rli_call(Host,Port,Goal,Result),Result=the(Goal).

rli_call_plain(Host,Port,Goal,Result):-invoke_rli_method(rli_call(Host,Port,Goal),Result).

rli_call(Host,Port,Goal,R):-
  val(rli,call,compressed),!,
  rli_call_compressed(Host,Port,Goal,R).
rli_call(Host,Port,Goal,R):-
  %val(rli,call,plain),!,
  rli_call_plain(Host,Port,Goal,R).  

% adds xwrap/xunwrap
rli_call_compressed(Host,Port,Goal,R):-
  xwrap(Goal,WG),
  rli_call_plain(Host,Port,call_xwrapped(WG,_),GX),
  ( GX=the(exception(E))->Result=E
  ; GX=the(call_xwrapped(_,WX))->xunwrap(WX,X),Result=the(X)
  ; Result=no
  ),
  ( Result=the(exception(Ex))->R=Ex
  ; R=Result
  ).

% encodes failure as exception
call_xwrapped(WG,WR):-xunwrap(WG,G),(G->R=G;R=exception(no)),xwrap(R,WR).

rli_call(Host,Port,X,Goal,R):-rli_call(Host,Port,X^Goal,Result),(Result=the(Y^_)->R=the(Y);R=Result).

rli_wait(Host,Port,Timeout,Bound):-invoke_rli_method(rli_wait(Host,Port,Timeout),Bound).

rli_wait(Host,Port,Timeout):-rli_wait(Host,Port,Timeout,Bound),Bound>0.

rli_wait(Port,Timeout):-rli_wait(localhost,Port,Timeout).

rli_wait(Port):-rli_wait(Port,0).

rli_hi(Port):-rli_call(Port,println('Hi!')).

rli_hi(Host,Port):-rli_call(Host,Port,println('Hi!')).

rli_in(Host,Port,Result):-invoke_rli_method(rli_in(Host,Port),Result).

rli_in(Port,Result):-rli_in(localhost,Port,Result).

rli_out(Port,Term):-rli_out(localhost,Port,Term).

rli_out(Host,Port,Term):-invoke_rli_method(rli_out(Host,Port,Term),_Result).

% discovers all available network interfaces on a computer
rli_get_inets(IAs):-
  %invoke_rli_method(rli_get_inets,Fun),
  call_java_class_method('rli.RLIAdaptor',rli_get_inets,Fun),
  Fun=..[_|IAs].

% run this once on the side with hidden or *UNKNOWN* IP address
% note that Host refers to the Host vith visible IP address

new_actor(Port):-bg(rli_run_server(Port)).

ask_actor(Port,Goal):-ask_actor(localhost,Port,Goal).

ask_actor(Host,Port,Goal):-ask_actor(Host,Port,Goal,the(Goal)).

ask_actor(Host,Port,Goal,Result):-rli_remote_run(Host,Port,Goal,Result).

% emulation of older run_server/remote_run protocol

rli_run_server(Port):-
  Host=localhost,
  port2ports(Port,InPort,OutPort),
  rli_start_server(InPort),
  rli_start_server(OutPort),
  rli_start_proxy_to(Host,InPort,OutPort,0).

rli_remote_run(Port,Goal):-rli_remote_run(localhost,Port,Goal).

rli_remote_run(Host,Port,Goal):-rli_remote_run(Host,Port,Goal,the(Goal)).

rli_remote_run(Host,Port,Goal,Result):-
   port2ports(Port,InPort,OutPort),
   rli_call_proxy(Host,InPort,OutPort,Goal,Result).

rli_wait_for(Host,Port):-
   port2ports(Port,InPort,OutPort),
   rli_wait(Host,InPort),
   rli_wait(Host,OutPort).

port2ports(Port,InPort,OutPort):-
  namecat('rli_',Port,'_in',InPort),
  namecat('rli_',Port,'_out',OutPort).

% RLI proxy API allowing tasks to be executed by agents with unknown IP addresses
  
rli_start_proxy_to(Host):-
  rli_start_proxy_to(Host,defInPort,defOutPort,0).

% variant, allowing multiple configurations each defined by an <InPort,OutPort>
rli_start_proxy_to(Host,InPort,OutPort,TimeOut):-
   rli_wait(Host,InPort,TimeOut),
   rli_wait(Host,OutPort,TimeOut),
   rli_proxy_to(Host,InPort,OutPort).

% variant, assuming the port have already been initialized
rli_proxy_to(Host):-rli_proxy_to(Host,defInPort,defOutPort).
   
rli_proxy_to(Host,InPort,OutPort):-
   repeat,
     rli_serve_proxy(Host,InPort,OutPort),
   fail.

% this will be run multiple times to pick a task and post an answer
rli_serve_proxy(Host,InPort,OutPort):-
  % println(proxi_waiting_for(Host)),
  rli_in(Host,InPort,Goal),
  (Goal->Result=the(Goal);Result=no),
  rli_out(Host,OutPort,Result).

% run this from client in the side with *VISIBLE* IP address to
% post a task to be solved by the side with unkonwn IP address
% run it each time you want a goal executed on the side with unknown IP address
rli_call_proxy(Goal):-rli_call_proxy(localhost,Goal).

rli_call_proxy(Host,Goal):-rli_call_proxy(Host,Goal,Result),Result=the(Goal).

% variant returning the remote result without binding Goal
rli_call_proxy(Host,Goal,Result):-
  rli_call_proxy(Host,defInPort,defOutPort,Goal,Result).

% variant allowing multiple configurations each defined by an <InPort,OutPort>
rli_call_proxy(Host,InPort,OutPort,Goal,Result):-
  rli_out(Host,InPort,Goal),
  rli_in(Host,OutPort,Result).

% run this on the side wit *VISIBLE* IP address, possibly on the client
rli_init_proxy:-rli_init_proxy(defInPort,defOutPort).

rli_init_proxy(InPort,OutPort):-
  rli_start_server(InPort),
  rli_start_server(OutPort).

  
% tentative new API ??

new_rserver(Port,PrologService):-
   this_class_object(Prolog),
   new_java_object('rli.RLIServer'(Port,Prolog,'$null','$null'),PrologService).

rserver_start(PrologService):-invoke_java_method(PrologService,start,_).

rserver_do(PrologService,Goal,Result):-
  invoke_java_method(PrologService,serverCallProlog(Goal),Result).

rserver_do(PrologService,Goal):-rserver_do(PrologService,Goal,Result),Result=the(Goal).

rserver_get_port(PrologService,P):-invoke_java_method(PrologService,getPort,P).

rserver_stop(PrologService):-invoke_java_method(PrologService,bg_stop,_).
  
% derived API

rli_stop_server(Port):-rli_stop_server(localhost,Port).

rli_call(Port,Goal):-rli_call(localhost,Port,Goal).

% defaults

default_rli_port(wormhole).

rli_start_server:-default_rli_port(Port),rli_start_server(Port).

rli_stop_server:-default_rli_port(Port),rli_stop_server(Port).

rli_call(Goal):-default_rli_port(Port),rli_call(Port,Goal).

toptest(G,Result):-
  new_java_class('prolog.kernel.Top',C),
  MethodAndArgs=call(G),
  functor(MethodAndArgs,F,_),
  invoke_java_method(C,C,F,MethodAndArgs,Result).

  
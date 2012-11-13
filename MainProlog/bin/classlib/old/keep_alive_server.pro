keep_alive_server:-default_port(P),keep_alive_server(P).

keep_alive_server(Port):-keep_alive_server(Port,none).

keep_alive_server(Port,Password):-
  integer(Port),Port>0,
  !,
  this_port<=Port,
  password<=Password,
  new_server(Port,Server),
  Server\=='$null',
  this_server<=Server.
keep_alive_server(_Port,_Password). % do nothing
  
serve:-
  this_server=>Server,
  !,
  password=>Password,
  repeat,
    (this_server=>S,S=='$null'-> !,fail;true),
    ( new_service(Server,Service)->
      bg(main_service_loop(Service,Password)),
      fail
    ; true
    ),
    !.
serve.

server_stop:-server_stop(none).

server_stop(Pwd):-
  password=>Pwd,
  this_server=>S,
  this_server<='$null',
  discontinue(S).

main_service_loop(Service,Password):-
  service_finished<=no,
  repeat,
    ( (service_finished=>yes)->true
    ; answer_one_query(Service,Password)->fail
    ; true
    ),
  !,
  disconnect(Service).
  
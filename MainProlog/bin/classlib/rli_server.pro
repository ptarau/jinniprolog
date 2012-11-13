rli_server:-default_server_port(P),rli_server(P).

default_server_port(P):-default_rli_port(P).

rli_server(Port):-
  server_port<=Port.

rli_server(Port,Password):-
  to_string(Port,S),
  namecat(S,Password,PW),
  rli_server(PW).
  
serve(Goal):-
  server_port=>Port,
  rli_start_server(Port),
  init_rli_server(Port,Goal).

serve(Goal,OutputArea):-
  server_port=>Port,
  output_area_to_pwriter(OutputArea,PWriter),
  rli_start_server(Port,PWriter),
  init_rli_server(Port,Goal).
  
init_rli_server(Port,Goal):-  
  rli_call(Port,server_port<=Port),
  rli_call(Port,Goal).

serve:-serve(println(server_running)).

server_stop:-
  server_port=>Port,
  rli_stop_server(Port).

server_stop(Pwd):-
  server_port=>Port,
  to_string(Port,S),
  namecat(S,Pwd,PW),
  rli_stop_server(PW).

%@@

/*
rli_server:-default_server_port(P),rli_server(P).

default_server_port(P):-default_rli_port(P).

rli_server(Port):-
  server_port<=Port.

rli_server(Port,Password):-
  to_string(Port,S),
  namecat(S,Password,PW),
  rli_server(PW).

serve(Goal):-
  server_port=>Port,
  % println(starting_rli_server(Port)),
  rli_start_server(Port),
  rli_call(Port,server_port<=Port),
  rli_call(Port,Goal).

serve:-serve(println(server_running)).

server_stop:-
  server_port=>Port,
  rli_stop_server(Port).

server_stop(Pwd):-
  server_port=>Port,
  to_string(Port,S),
  namecat(S,Pwd,PW),
  rli_stop_server(PW).

%@@

*/
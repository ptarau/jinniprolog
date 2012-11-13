:-[rli_server].

% start_registry:-rli_init.

server:-rli_server.

server(Port):-rli_server(Port).

server(Port,Password):-rli_server(Port,Password).

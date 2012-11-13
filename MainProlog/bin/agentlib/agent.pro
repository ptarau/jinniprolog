/*
  Basic Agent Class. Encapsulates P2P abilities
  (client + server) and goal oriented behavor. 
*/

agent(Port):-agent(Port,true).

/*
   robust agent constructor initialized with a Goal
   note no binding is propageted back to goal
   and failure in Goal is ignored !!!
*/

% was Goal+Port - check all instances

agent(Port,Goal):-
  Host=localhost,
  rli_start_server(Port),
  rli_call_plain(Host,Port,server_port<=Port,_),
  rli_call_plain(Host,Port,Goal,_).

  
% agent messaging

out_mes(To,Mes):-server_port=>From,out_mes(From,To,Mes).

out_mes(From,To,Mes):-rli_out(To,mes(From,Mes)).

in_mes(From,Mes):-server_port=>To,in_mes(From,To,Mes).

in_mes(From,To,Mes):-rli_in(To,mes(From,Mes)).

default_broker(localhost,broker,'').

% messaging API

rli_send(To,Mes):-out_mes(To,Mes).

rli_reply_with(Action):-
  in_mes(From,Mes),
  ( call(Action,Mes,Result)->rli_send(From,the(Result))
  ; rli_send(From,no)
  ).
  
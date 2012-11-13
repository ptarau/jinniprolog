/*
  Basic Agent Class. Encapsulates P2P abilities
  (client + server) and goal oriented behavor. 
*/

actor(Port):-actor(Port,true).

/*
   robust agent constructor initialized with a Goal
   note no binding is propageted back to goal
   and failure in Goal is ignored !!!
*/

actor(Port,Goal):-
  server_port<=Port,
  new_actor(Port),
  bg(Goal).
  

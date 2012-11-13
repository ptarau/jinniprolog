:-[registered_agent].


/*
  Register Agent - facilitates communication between
  agents by providing a registry service.
 
  The mechanism is simple - and designed to support
  thousands of mobile agents.
  
  Each agent requests a unique ID from the register.
  Agents are also remembered through their claimed names,
  provided that they provide a matching host and port address.
*/

start:-that_host=>H,this_port=>P,println(peer(H)+port(P)).

registrar:-
  default_registry(_Host,Port,Pwd),
  registrar(Port,Pwd).

registrar(Port,Password):-
  agent(true,Port,localhost,Port,Password).

register(IdLoc):-register(_noname,localhost,IdLoc).

register(Name,IdLoc):-register(Name,localhost,IdLoc).

register(Name,Host,IdLoc):-register(Name,Host,_Port,IdLoc).

register(Name,Host,Port,IdLoc):-
  find_uid(Name,Host,Port,IdLoc),
  !.
register(Name,Host,Port,IdLoc):-
  var(Port),nonvar(Host),
  !,
  (val(Host,P)->true;this_port=>P),
  Port is P+1,
  ensure_named(Host,Port,Name),
  let(Host,Port),
  new_uid(Id),
  IdLoc=..[Id,Name,Host,Port],
  db_assert('$id',IdLoc),
  db_assert(Name,IdLoc).
register(Name,Host,Port,_IdLoc):-
  println(bad_arguments:cannot_register(Name,Host,Port)),
  fail.

ensure_named(_Host,_Port,Name):-nonvar(Name),!.
ensure_named(Host,Port,Name):-namecat('agent_at_',Host,Port,Name).

db2uid(IdLoc):-db_clause('$id',IdLoc,_).
  
find_uid(Name,Host,Port,IdLoc):-
  nonvar(Name),
  !,  
  db_clause(Name,IdLoc,_),
  IdLoc=..[_Id,Name,Host,Port].
find_uid(Name,Host,Port,IdLoc):-
  nonvar(Host),nonvar(Port),
  db2uid(IdLoc),
  IdLoc=..[_Id,Name,Host,Port].
  
find_uids(Name,IdLocs):-find_uids(Name,_Host,IdLocs).

find_uids(Name,Host,IdLocs):-find_uids(Name,Host,_Port,IdLocs).
 
find_uids(Name,Host,Port,IdLocs):-
  findall(IdLoc,find_uid(Name,Host,Port,IdLoc),IdLocs).

wait_for_ad(Pred):-repeat,sleep(1),get_ad(Pred),!.

new_ad(Pred):-get_ad(Pred),!.
new_ad(Pred):-db_assert(advertized,Pred).

get_ad(Pred):-db_clause(advertized,Pred,true),!.

get_all_ads(Pred,Ps):-findall(Pred,db_clause(advertized,Pred,true),Ps).

% end
  
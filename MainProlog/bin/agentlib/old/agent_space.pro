:-[registrar].
:-[cat].

/*
  Agent Space - facilitates communication between
  agents by providing an information exchange and coordination
  framework.
  
  This also provides registry services.
  
  By including agent state - the registrar db as well as 
  the communication flow can be recorded and possible visualized
  as a Prolog Cat object.
*/

db2uid(Id,Name,Host,Port):-
 db2uid(IdLoc),
 IdLoc=..[Id,Name,Host,Port].
 
db2props_cat(C):-
  new_cat(C),
  foreach(
    db2uid(Id,Name,Host,Port),
    ( set_prop(C,Id,name,Name),
      set_prop(C,Id,host,Host),
      set_prop(C,Id,port,Port)
    )
  ).

db2links_cat(C):-
  new_cat(C),
  foreach(
    db2uid(Id,Name,Host,Port),
    ( set_prop(C,Id,name,Name),
      set_prop(C,Host,host,Host),
      %set_prop(C,Host,Port,Port),
      set_hyper(C,Host,3),
      set_morphism(C,Host,Id,Name,Port)
    )
  ).  

show_in_window:-
  new_frame(Frame),
  show_in_window(Frame).
  
show_in_window(Frame):-
  new_console(Frame,monitor,20,10,_Console),
  show(Frame).
   
monitor:-monitor(10000).

monitor(Max):-
  for(_,1,Max),
  sleep(2),
  show_agents,
  fail
; true.
  
show_agents:-
  foreach(
    db2uid(Id,Name,Host,Port),
    (
      println([uid=Id,name=Name,host=Host,port=Port])
    )
  ).

show3d:-
  db2links_cat(Cat),
  prolog3d:show3d(Cat).

renshow:-
  db2links_cat(Cat),
  renagent:renshow(Cat).

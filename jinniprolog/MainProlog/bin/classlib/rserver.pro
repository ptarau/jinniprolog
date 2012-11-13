:-[java_object].

rserver:-default_rli_port(P),rserver(P).

rserver(Port):-
  this_class_object(Prolog),
  java_object('rli.RLIServer',args(Port,Prolog)).

serve:-invoke(start).

rdo(Goal,Result):-invoke(serverCallProlog(Goal),Result).

rget_port(Port):-invoke(getPort,Port).

rstop:-invoke(bg_stop).


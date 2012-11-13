get_class_name(Name):-jcall(get_class_name,any,Name).
  
get_class_name(Engine,Name):-
  handle2object(Engine,O),
  invoke_java_method(O,'get_class_name',void,Name).
  
get_class_id(ID):-this_class(ID).

get_class_id(E,ID):-
  handle2object(E,O),
  invoke_java_method(O,'get_class_id',void,ID).
  
% set_instance_id(ID):-current_engine(E),set_instance_id(E,ID).

set_instance_id(Engine,ID):-
  handle2object(Engine,O),
  invoke_java_method(O,'set_instance_id',args(ID),_).

engine_set_input(M,Handle):-invoke_java_method(M,set_input(Handle),_).

engine_set_output(M,Handle):-invoke_java_method(M,set_output(Handle),_).

runnable_engine(G,Runnable):-
  new_engine(true,G,E),
  handle2object(E,Runnable).
    
current_engine_object(O):-
  current_engine(E),
  handle2object(E,O).
 
set_local_prop(K1,K2,V):-
  this_class_object(Prolog),
  invoke_java_method(Prolog,setProp(K1,K2,V),_).
    
get_local_prop(K1,K2,V):-
  this_class_object(Prolog),
  invoke_java_method(Prolog,getProp(K1,K2),V),
  V \== '$null'.

clear_local_props:-
  this_class_object(Prolog),
  invoke_java_method(Prolog,clearProps,_).
  
local_prop_iterator(I):-
  this_class_object(Prolog),
  invoke_java_method(Prolog,propIterator,I).

local_prop(K1,K2,V):-
  local_prop_iterator(I),
  iterator_element(I,K),
  invoke_java_method(K,getTerm,R),
  R=x(K1,K2),
  get_local_prop(K1,K2,V).
  
    
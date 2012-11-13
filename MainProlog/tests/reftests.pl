% :-[refl].

etest:-
  new_java_class('java.util.Vector',C),
  new_java_object(C,void,V),
  invoke_java_method(V,addElement(hello),_),
  invoke_java_method(V,addElement(bye),_),
  invoke_java_method(V,elements,E),
  new_java_class('java.util.Enumeration',EC),
  invoke_java_method(EC,E,nextElement,void,X),
  invoke_java_method(EC,E,nextElement,void,Y),
  println(X+Y).
  
btest:-
  new_java_class('prolog.kernel.JavaIO',O),  
  get_java_field_handle(O,showOutput,Field),
  invoke_java_method(Field,get(O),R1),
  invoke_java_method(Field,set(O,R1),R2),
  invoke_java_method(Field,get(O),R3),
  println(field=Field+R1+R2+R3).
  
g1:-for(_,1,100000),out(a),in(a),fail.
  
g2:-out(a(99)),in(a(_)),g2.
  
tg0:-
  hub_ms(0,H),
  ( 
    eq(T,99), % only atomic data !!!
    println(sent(T)),
    bg(and(sleep(3),hub_put(H,T)),_),
    fail
  ; hub_collect(H,X),
    println(received(X))
  ).
  
tg1:-
  bg(and(in(2000,a(X,X),R),println(in(a(X,X),R))),T),
  println(launched(T)),
  sleep_ms(6000),
  out(a(99,_),R),
  println(out(R)).
  
tg2:-
  bg(and(sleep(1),notify_about(a(30)))),
  bg(and(sleep(5),notify_about(a(10)))),
  wait_for(a(X),(X<20,println(here(X))),R),
  wait_for(a(X),(X<40,println(there(X))),RR),
  println(R),
  println(RR).
    

get_java_fd(O,FieldName,Field):-
  invoke_java_method(O,getClass,void,C),
  println(C),
  invoke_java_method(C,getClass,void,CC),
  println(CC),
  invoke_java_method(C,getField(FieldName),F),
  println(F),
  invoke_java_method(F,get(O),Field).

bug1:-
  % new_client(localhost,80,O),
  new_java_object('java.lang.Integer'(99),O),
  println(o=O),
  get_java_fd(O,host,Field),
  println(field=Field).


go4:-
  % new_client(localhost,80,O),
  new_java_object('prolog.core.Transport'(localhost,80),O),
  println(o=O),
  get_java_field_handle(O,port,Field),
  invoke_java_method(Field,get(O),R1),
  invoke_java_method(Field,set(O,99),R2),
  invoke_java_method(Field,get(O),R3),
  println(field=Field+R1+R2+R3).
  
go3:-
  % new_client(localhost,80,O),
  new_java_object('prolog.core.Transport'(localhost,80),O), % make sure server is there
  println(o=O),
  get_java_field_handle(O,port,Field),
  invoke_java_method(Field,get(O),R1),
  invoke_java_method(Field,set(O,99),R2),
  invoke_java_method(Field,get(O),R3),
  println(field=Field+R1+R2+R3).
  
go2:-
  new_java_class('java.util.Hashtable',C),
  new_java_object(C,void,O),
  println(C+O),
  invoke_java_method(O,toString,void,SO),
  println(SO),
  invoke_java_method(O,put,args(hello,bye),R),
  invoke_java_method(O,toString,void,S1),
  println(R+S1).
  
go1:-
  new_java_class('java.util.Hashtable',C),
  new_java_object(C,void,O),
  println(C+O),
  invoke_java_method(O,toString,void,SO),
  println(SO),
  invoke_java_method(O,put,args(hello,bye),R),
  println(R).

go0:-
  new_java_class('java.lang.Thread',C),
  invoke_java_method(C,currentThread,void,T),
  invoke_java_method(T,isAlive,void,R),
  invoke_java_method(T,sleep,arg(500),V),
  println(V+R).
  
  
  
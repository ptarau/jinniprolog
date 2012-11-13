:-[java_object].

java_array(ArrayHandle):-
  object<=ArrayHandle.
  
java_array(ClassName,Len):-
  new_java_class(ClassName,BaseClass),
  invoke_array_method(newInstance(BaseClass,Len),ArrayHandle),
  object<=ArrayHandle.

get_length(L):-
  object=>Array,
  invoke_array_method(getLength(Array),L).

array_set(I,X):-
  object=>Array,
  invoke_array_method(set(Array,I,X),_).

array_get(I,X):-
  object=>Array,
  invoke_array_method(get(Array,I),X).
      
/* usage:

?- new(java_array('java.lang.String',5),S),S:get_length(L).
L = 5 S = '$instance'('$object'(1086),2) ;

?- new(java_array('java.lang.String',5),S),S:array_set(0,'hello'),S:array_get(0,X).

S = '$instance'('$object'(1088),1) X = hello ;
no

*/            
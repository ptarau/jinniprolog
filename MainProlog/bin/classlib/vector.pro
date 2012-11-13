/*
 Example of wrapper for a Java class as a Prolog class
*/

:-[java_object].

vector:-
  new_java_class('java.util.Vector',C),
  new_java_object(C,void,JavaVector),
  object<=JavaVector.

vector(JavaVector):-
  object<=JavaVector.
  
vector(list,L):-
  vector,
  from_list(L).

add_data(Data):-
  object=>V,
  invoke_java_method(V,addElement(Data),_Result).
  
get_data(Index,Data):-
  object=>V,
  invoke_java_method(V,elementAt(Index),Data).
 
set_data(Index,Data):-
  object=>V,
  invoke_java_method(V,setElementAt(Data,Index),_Result).

/* % deprecated
to_list(L):-
  current_engine_object(M),
  object=>V,
  invoke_java_method(M,vector2list(V),L).
*/

to_enumeration(E):-
  object=>V,
  invoke_java_method(V,elements,VE),
  new(enumeration(VE),E).
  
from_list(L):-
  map(add_data,L).
    
test:-
  add_data(hello),
  add_data(f(X,X)),
  add_data(g(X,X)),
  add_data(bye),
  to_list(L),
  println(L),
  to_enumeration(E),
  repeat,
    ( E:hasMoreElements->E:nextElement(X),println(enum=X),fail
    ; true
    ),
    !,
  object=>JavaVector,
  new(vector(JavaVector),V), % cloning
  V:to_list(List),
  println(cloned=List).
  
     
/*

?- new(vector,V),V:add_data(hello),
   V:add_data(bye),V:get_data(0,X),V:toString(S).
   
V = '$instance'(vector@1084,1) S = '[hello, bye]' X = hello ;
no

*/
   
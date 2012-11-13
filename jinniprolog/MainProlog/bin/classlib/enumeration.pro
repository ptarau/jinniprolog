/*
 Example of wrapper for a Java interface as a Prolog class
*/

:-[interface].

enumeration(Enumerable):-
  interface('java.util.Enumeration',Enumerable).
  
nextElement(X):-
  class=>EC,
  object=>E,
  invoke_java_method(EC,E,nextElement,void,X).
  
hasMoreElements:-
  class=>EC,
  object=>E,
  invoke_java_method(EC,E,hasMoreElements,void,YesNo),
  new_java_object('java.lang.Boolean'(true),YesNo).
   
backtrack_at(X):-
  repeat,
  ( hasMoreElements->nextElement(X)
  ; !,fail
  ).
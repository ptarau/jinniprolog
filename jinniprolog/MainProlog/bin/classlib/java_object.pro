/*
 Example of wrapper for a Java class as a Prolog class.
 This is actually a wrapper for Java's own Object class.
 By inheriting from it, a Prolog class used as a wrapper
 for a Java class can access the (essential) services
 offered by the class Object in Java.
*/

java_object:-java_object('java.lang.Object',void).

java_object(ClassName):-
  java_object(ClassName,void).

java_object(ClassName,Constructor):-
  new_java_class(ClassName,C),
  new_java_object(C,Constructor,O),
  object<=O.

invoke(Method):-invoke(Method,_Result).

invoke(Method,Result):-
  object=>O,
  invoke_java_method(O,Method,Result).

equals(Other):-
  Other:(object=>OtherO),
  invoke(equals(OtherO),TF),
  invoke_java_method(TF,toString,true).
         
toString(S):-
  invoke(toString,S).
     
/*

?- new(java_object,J),J:equals(J),J:toString(S),println(S).
*/
   
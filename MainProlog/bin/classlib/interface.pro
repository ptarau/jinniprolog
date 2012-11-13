/*
 Example of wrapper for a Java interface as a Prolog class
*/

:-[java_object].

interface(Name,InterfacableObject):-
  new_java_class(Name,IC),
  object<=InterfacableObject,
  class<=IC.

   
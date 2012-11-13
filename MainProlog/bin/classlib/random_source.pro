/*
 Example of wrapper for a Java class as a Prolog class
 Uses some functionality inherited from the wrapper
 java_object which reflects the Java calls "Object"

*/

:-[java_object].

random_source:-random_source(13).

random_source(Seed):-random_source(r,Seed).

random_source(Sym,Seed):-
  new_java_object('java.util.Random'(Seed),Random),
  seed<=Seed,
  sym<=Sym,
  object<=Random.

next_int(I):-
  object=>T,
  invoke_java_method(T,nextInt,X),
  abs(X,I).

next_sym(S):-
  sym=>Sym,
  next_int(I),
  symcat(Sym,I,S).
  
reset:-
  object=>T,
  seed=>Seed,
  invoke_java_method(T,setSeed(Seed),_).
  

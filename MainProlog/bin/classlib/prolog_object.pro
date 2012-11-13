/*
  Root class for some basic Prolog class/object services
  This provides only a few services - as the complete set of
  Prolog builtins is available in any class.
  Note that arity 0 constuctors are automatically called
  after a class is loaded.
*/

equals(This):-
  this(This).
         
toString(S):-
  this('$instance'(_Class,OID)),
  %arg(1,Class,CID),
  namecat(prolog_object,'_',OID,S).
  
/*
?- new(prolog_object,J),J:equals(J),J:toString(S),println(S).
*/
   
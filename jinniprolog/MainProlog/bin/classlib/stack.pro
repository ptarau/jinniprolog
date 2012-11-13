stack :- clear.

isEmpty:-
  handle=>[].

push(X):-
  handle=>Xs,
  handle<=[X|Xs].

pop(X):-
  handle=>[X|Xs],
  handle<=Xs.

cloneFrom(OtherStack):-
  OtherStack:(handle=>Xs),
  handle<=Xs.

clear:-
  handle<=[].

getElements(Xs):-
  handle=>Xs.

anElement(X):-
   getElements(Xs),
   member(X,Xs).
   
show:-
  handle=>Xs,
  println(Xs).

  
/*
?- new(stack,S),S:push(a),S:push(b),S:show.
[b,a]
S = '$instance'(stack@958,3) ;
no
*/

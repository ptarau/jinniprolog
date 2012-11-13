g(d,l).
g(l,a).
g(l,c).
g(a,r).
g(a,c).
g(r,p).

g(f,gp).

sg(X,Y):-g(X,Y).
sg(X,Y):-g(Y,X).

tg(From,To):-tg(From,To,[]).

tg(X,X,Xs):- \+(member_of(Xs,X)).
tg(X,Z,Xs):-
  \+(member_of(Xs,X)),
  sg(X,Y),
  add(X,Xs,Ys),
  tg(Y,Z,Ys).

/*
a member is the head X or
  a member of the tail Xs
*/
member_of([X|_],X).
member_of([_|Xs],X):-
  member_of(Xs,X).

add(X,Xs,[X|Xs]).

% :-println(hello),tg(X,Y),println(X+Y),fail.



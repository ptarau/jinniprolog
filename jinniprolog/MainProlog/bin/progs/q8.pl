% Variant of a program by T. Fruhwrith

go:-go(_queen),write(x),fail.
go:-nl.

go(Qs):-queens(8,Qs).

queens(N,Ps):-
  gen_places(N,Ps),
  gen_queens(N,Qs),
  place_queens(Qs,Ps,_,_).

% at each step inc Us and dec Ds
place_queens([],_,_,_).
place_queens([I|Is],Cs,Us,[_|Ds]):-
  place_queens(Is,Cs,[_|Us],Ds),
  place_queen(I,Cs,Us,Ds).

place_queen(I,[I|_],[I|_],[I|_]).
place_queen(I,[_|Cs],[_|Us],[_|Ds]):-
  place_queen(I,Cs,Us,Ds).

gen_places(Max,Ps):-
  findall(_,for(_,1,Max),Ps).

gen_queens(Max,Qs):-
  findall(Q,for(Q,1,Max),Qs).

qs(Ps):-
  eq(Ps,[_1,_2,_3,_4,_5 ,_6,_7,_8,_9]),
  place_queens([1,2,3,4,5,6,7,8,9],Ps,_,_).


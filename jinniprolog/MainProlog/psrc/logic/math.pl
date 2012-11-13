/*
 if compiled with bp a different $float will be used !!!
*/

% complement
'\'(B,C):-'\'(0,B,C).

max(X,Y,Z):-X>Y,!,Z=X.
max(_,Y,Y).

min(X,Y,Z):-X<Y,!,Z=X.
min(_,Y,Y).

ints([],I,I):-!.
ints([I0|L],I0,I):-I0<I,I1 is I0+1,ints(L,I1,I).

random_seed(ID):-jcall(random_seed,ID,_).


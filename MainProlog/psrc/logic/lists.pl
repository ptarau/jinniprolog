% basics

map(F,Xs,Ys,Zs):-map0(Xs,F,Ys,Zs).

map0([],_,[],[]).
map0([X|Xs],F,[Y|Ys],[Z|Zs]):-
   call(F,X,Y,Z),
   map0(Xs,F,Ys,Zs).
   

map(F,Xs,Ys):-map0(Xs,F,Ys).

map0([],_,[]).
map0([X|Xs],F,[Y|Ys]):-
   call(F,X,Y),
   map0(Xs,F,Ys).

map(F,Xs):-map0(Xs,F).

map0([],_).
map0([X|Xs],F):-
   call(F,X),
   map0(Xs,F).
    
foldl(F,Z,Xs,R):-foldl0(Xs,F,Z,R).
  
foldl0([],_,R,R).
foldl0([X|Xs],F,R1,R3):-call(F,R1,X,R2),foldl0(Xs,F,R2,R3).

foldr(F,Z,Xs,R):-foldr0(Xs,F,Z,R).
  
foldr0([],_,Z,Z).
foldr0([X|Xs],F,Z,R2):-foldr0(Xs,F,Z,R1),call(F,X,R1,R2).

sum(Xs,R):-foldl('+',0,Xs,R).

prod(Xs,R):-foldl('*',1,Xs,R).

% LIST PROCESSING

append([],Ys,Ys).
append([X|Xs],Ys,[X|Zs]):-append(Xs,Ys,Zs).

appendN(Xss,Xs):-foldl(det_append,[],Xss,Xs).

select(X,[X|S],S).
select(X,[Y|S1],[Y|S2]):- %nonvar(S1),
  select(X,S1,S2).

member(X,[X|_]).
member(X,[_|Xs]):-member(X,Xs).

rmember(X,[_|Xs]):-rmember(X,Xs).
rmember(X,[X|_]).

rselect(X,[X|S],S).
rselect(X,[Y|S1],[Y|S2]):- %nonvar(S1),
  rselect(X,S1,S2).
  
nth_member(X,Xs,N):-member_i(X,Xs,1,N).

nth_member0(X,Xs,N):-member_i(X,Xs,0,N).

member_i(X,[X|_],N,N).
member_i(X,[_|Xs],N1,N3):-
  '+'(N1,1,N2),
  member_i(X,Xs,N2,N3).

reverse(Xs,Ys):-rev(Xs,[],Ys).

rev([],Ys,Ys).
rev([X|Xs],Ys,Zs):-rev(Xs,[X|Ys],Zs).

length(L,N):-var(N),!,get_length(L,0,N).
length(L,N):-make_length(L,0,N).

get_length([],I,I).
get_length([_|L],I0,I):-I1 is I0+1,get_length(L,I1,I).

make_length([],I,I):-!.
make_length([_|L],I0,I):-I0<I,I1 is I0+1,make_length(L,I1,I).


numlist(I,I,[]):-!.
numlist(I0,I,[I0|Is]):-I0<I,I1 is I0+1,numlist(I1,I,Is).

tsub_of(T,S):-tsub_of(8,T,S).

tsub_of(0,T,T).
tsub_of(K,T,S):-K>0,K1 is K-1,tsub1_of(T,S1),tsub_of(K1,S1,S).
  
tsub1_of(T,S):-
  functor(T,F,N),
  functor(S,F,N),
  for(I,1,N),
   arg(I,T,A),
   arg(I,S,SA),
   functor(A,FA,NA),
   functor(SA,FA,NA),
   I1 is I+1,
   ( for(J,I1,N),
       arg(J,T,B),
       arg(J,S,SB),
       functor(B,FB,NB),
       functor(SB,FB,NB)
     ; true
   ).
   
tg:-
  T=f(a,g(b,c),h(d),_,_),
  println(T),
  tsub1_of(T,S),
  tab(2),println(S),
  fail
; true.

/*
- a superterm is obtained by replacing a functor or atom at a give level
  with a variable
- two term are unifiable iff they have at least 2 equal super terms
*/

% iss(Is,Js1,Js2):-select(_,Is,Js),(Js1=Is,Js2=Js;iss(Js,Js1,Js2)).

%iss(Is,Js1,Js2):-select(_,Is,Js),iss1(Is,Js,Js1,Js2).
%iss1(Is,Js,Js1,Js2):-Js1=Is,Js2=Js;iss(Js,Js1,Js2).

igo:-
  for(I,1,8),
  igo(I,L),
  println(I=>L),
  fail
; true.

igo(N,L):-
  length(Is,N),
  findall(_,iss(Is,_Sup,_Sub),Xs),
  length(Xs,L),println(L).
  

iss(Is,Js1,Js2):-
 select(_,Is,Js),
 iss1(Is,Js,Js1,Js2).

iss1(Is,Js,Is,Js).
iss1(_,Is,Js1,Js2):-select(_,Is,Js),iss1(Is,Js,Js1,Js2).


css(Is,Js1,Js2):-
  reverse(Is,Js),
  css2(Js,Rs1,Rs2),
  reverse(Rs1,Js1),
  reverse(Rs2,Js2).
  
css2(Is,Js1,Js2):-
 tail_of(Is,Js),
 css1(Is,Js,Js1,Js2).

css1(Is,Js,Is,Js).
css1(_,Is,Js1,Js2):-tail_of(Is,Js),css1(Is,Js,Js1,Js2).

tail_of([_|Xs],Xs).

subset_of([],[]).
subset_of([I|Is],[I|Js]):-subset_of(Is,Js).
subset_of([_|Is],Js):-subset_of(Is,Js).  

isubset_of(As,Bs-Cs):-
 reverse(As,Xs),
 isubset_of(Xs,Ys,Zs),
 reverse(Zs,Cs),
 reverse(Ys,Bs).

isubset_of([],[],[]).
isubset_of([I|Is],[I|Js],Js):-isubset_of(Is,Js,_).
isubset_of([_|Is],Js,Ks):-isubset_of(Is,Js,Ks).  

%unpair(I,A,B):-cantor_unpair(I,A,B).
%pair(A,B,I):-cantor_pair(A,B,I).

unpair(I,A,B):-bitmix_unpair(I,A,B).
pair(A,B,I):-bitmix_pair(A,B,I).

pairop(Op,I,R):-unpair(I,A,B),call(Op,A,B,R).

psum(I,R):-pairop('+',I,R).
pprod(I,R):-pairop('*',I,R).

unpair(Op,I,A,B):-call(Op,I,A,B).

unpair_tree(Max,I,R):-unpair_tree(bitmix_unpair,Max,I,R).

unpair_tree(_Op,Max,X,R):-X<Max,R=X.
unpair_tree(Op,Max,N,t(X,Y)):-N>=Max,
  unpair(Op,N,A,B),
  unpair_tree(Op,Max,A,X),
  unpair_tree(Op,Max,B,Y).

cprod_pair(I,J,P):-
  int2exps(I,Is),
  int2exps(J,Js),
  member(X,Is),
  member(Y,Js),
  pair(X,Y,P).

cantor_pair(X,Y,P):-S is X+Y,Q is S*S,P is (Q+(3*X)+Y)//2.
%cantor_pair(K1,K2,P):-P is (((K1+K2)*(K1+K2+1))//2)+K2.
%cantor_pair(X,Y,P):-P is (X*X+X+2*X*Y+3*Y+Y*Y)//2.

cantor_tree(0,0).
cantor_tree(1,1).
cantor_tree(N,t(X,Y)):-N>1,cantor_unpair(N,A,B),cantor_tree(A,X),cantor_tree(B,Y).

cantor_unpair(Z,K1,K2):-I is floor((sqrt(8*Z+1)-1)/2),K1 is Z-((I*(I+1))//2),K2 is ((I*(3+I))//2)-Z.

cantor_pair:-cantor_pair(5).

cantor_pair(N):-
  for(I,0,N),
  for(J,0,N),
  cantor_pair(I,J,P),
  cantor_unpair(P,A,B),
  I==A,J==B,
  println(I+J=>P),
  fail.

cantor_unpair:-cantor_unpair(50).

cantor_unpair(N):-
  for(P,0,N),
  cantor_unpair(P,A,B),
  cantor_pair(A,B,Q),
  P==Q,
  println(P=>A+B),
  fail.

int2bxcat(N,C):-int2bxcat(N,2,C). % loops if 1 - feature !!!

int2bxcat(N,Max,C):-
  new_cat(C),
  foreach(
    int2bx(N,Max,e(A,B)),
    set_morphism_and_color(C,A,B)
  ).

% todo: use for syntehsis !!!
int2bx(N,P):-int2bx(N,2,P). % loops if 1 !!!

int2bx(N,Max,P):-N>=Max,
  bitmix_unpair(N,E1,E2),
  member(E,[E1,E2]),
  ( P=e(N,E)
  ; int2bx(E,Max,P)
  ).
  
bitmix_tree(0,0).
bitmix_tree(1,1).
bitmix_tree(N,t(X,Y)):-N>1,bitmix_unpair(N,A,B),bitmix_tree(A,X),bitmix_tree(B,Y).

/*
bitmix_eval(0,0).
bitmix_eval(1,1).
bitmix_eval(N,t(X,Y)):-N>1,bitmix_unpair(N,A,B),bitmix_tree(A,X),bitmix_tree(B,Y).
*/

bitmix_pair(N):-
  for(I,0,N),
  for(J,0,N),
  bitmix_pair(I,J,P),
  bitmix_unpair(P,A,B),
  I==A,J==B,
  println(I+J=>P),
  fail.

bitmix_unpair(N):-
  for(P,0,N),
  bitmix_unpair(P,A,B),
  bitmix_pair(A,B,Q),
  P==Q,
  println(P=>A+B),
  fail.
    
bitmix_pair(Y,X,P):-int2rbs(X,Xs),int2rbs(Y,Ys),bitmix(Xs,Ys,Ps),rbits2int(Ps,P).

bitmix_unpair(P,Y,X):-int2rbs(P,Ps),bitmix(Xs,Ys,Ps),rbits2int(Xs,X),rbits2int(Ys,Y),!.

bitmix([],[],[]).
bitmix([],[Y|Ys],[A,B|Ps]):-bitmix2(0,Y,A,B),bitmix([],Ys,Ps).
bitmix([X|Xs],[],[A,B|Ps]):-bitmix2(X,0,A,B),bitmix(Xs,[],Ps).
bitmix([X|Xs],[Y|Ys],[A,B|Ps]):-bitmix2(X,Y,A,B),bitmix(Xs,Ys,Ps).

bitmix2(X,Y,X,Y).

%bitmix2(X,Y,A,Y):-bitx(X,Y,A).
%bitmix2(X,Y,X,B):-bitx(X,Y,B).

bitx(0,0,0).
bitx(0,1,1).
bitx(1,0,1).
bitx(1,1,0).

int2rbs(N,Bs):-int2rbs(N,0,Bs).

int2rbs(0,K,Bs):-J is K mod 2,odd1ev0(J,Bs).
int2rbs(N,K,[B|Bs]):-N>0,B is N mod 2, N1 is N//2,K1 is K+1,int2rbs(N1,K1,Bs).

odd1ev0(0,[]).
odd1ev0(1,[0]).

int2graph(N,C):-
  new_cat(C),
  (
    int2pairs(N,e(A,B)),
    set_morphism_and_color(C,A,B,f,t,0),
    fail
  ; true  
  ).
    
int2pairs(N,P):-int2pairs(N,_E,P).   
    
int2pairs(N,E,e(A,B)):-
  int2exps(N,Es),
  member(E,Es),
  bitmix_unpair(E,A,B).  

bigint2graph(N,C):-
  new_cat(C),
  (
    bigint2pairs(N,e(A,B)),
    set_morphism_and_color(C,A,B),
    fail
  ; true
  ).
  
bigint2pairs(N,P):-bigint2pairs(N,_E,P).

bigint2pairs(N,E,e(A,B)):-
  bigint2exps(N,Ts),
  array_to_list(Ts,Es),
  member(E,Es),
  bigint2tuple(E,Xs),
  array_to_list(Xs,Ps),
  Ps=[A,B]. 
  
bigint2bigexp(B,E):-
  bigint2exps(B,Ts),
  array_to_list(Ts,Es),
  member(E,Es).
    
% converts an int N into Bits ints as a bijection from Nat->Nat^N  
int2tuple(Bits,N,Rs):-Base is 1<<Bits,int2tuple(Bits,Base,N,Rs).

int2tuple(Bits,Base,N,Rs):-int2base(Base,N,Ds),harvesteach(Bits,Ds,Rs).

harvesteach(0,_Ds,[]).
harvesteach(I,Ds,[R|Rs]):-I>0,I1 is I-1,harvestnth(I1,Ds,R),harvesteach(I1,Ds,Rs).

harvestnth(I,Ds,R):-map(harvestexp(I),Ds,Bs),rbits2int(Bs,R).

harvestexp(I,N,R):-getrbit(N,I,1),!,R=1.
harvestexp(_I,_N,0).

% converts N to a list of digits in a given Base
int2base(Base,N,Bs):-int2base(N,Base,0,Bs).

int2base(N,R,_K,Bs):-N<R,Bs=[N].
int2base(N,R,K,[B|Bs]):-N>=R,B is N mod R, N1 is N//R,K1 is K+1,int2base(N1,R,K1,Bs).

/*
% converts N to an enumeration of digits in a given Base
int2npair(Bits,N, K,R):-Base is 1<<Bits,int2npair(Bits,Base,N,K,R).

int2npair(Bits,Base,N,K,R):-int2base(Base,N,Ds),for(K,1,Bits),I is Bits-K,harvestnth(I,Ds,R).
*/

int2rpair(Bits,N,P):-
  int2tuple(Bits,N,Ns),
  (  expandable_tuple(N,Ns)->
     nth_member0(N1,Ns,K),
    ( P=e(N,K,N1)
    ; int2rpair(Bits,N1,P)
    )
  ; P=l(N)  
  ).

expandable_tuple(N,Ns):-for_all(member(N1,Ns),N1<N).

int2ncat(Bits,N,C):-
  new_cat(C),
  set_prop_and_color(C,N,3),
  ( int2rpair(Bits,N,P),
    println(P),
    ( P=l(V)->set_prop_and_color(C,V,1)
    ; P=e(F,K,T),
      namecat(F,':',K,FK),
      set_prop_and_color(C,F,0),
      set_prop_and_color(C,FK,2),
      set_morphism_and_color(C,F,FK),
      set_morphism_and_color(C,FK,T)
    ),
    fail
  ; true
  ).
  
rbit(I,N,B):-getrbit(N,I,B).
  
tuple2int(Ns,Bs):-
  length(Ns,Bits),M is Bits-1,
  for(I,0,M),
  map(rbit(I),Ns,Bs).

bigran:-bigran(10).

bigran(Bits):-
  random_bigint(Bits,B),
  bshow(B).

brantest:-brantest(32).

brantest(N):-brantest(N,_C).

brantest(N,C):-
  random_bigint(N,B),
  bigint_print(B),nl,
  bigint2igraph(B,C),
  igraph2bigint(C,BB),
  dualize(C),
  igraph2bigint(C,D),
  nl,bigint_print(D),nl,
  nl,bigint_print(BB),nl.

branshow:-branshow(128).

branshow(Bits):-
  brantest(Bits,C),
  rshow(C).

% hereditarily finite set duality 
hdual(B,D):-
  bigint2igraph(B,C),
  dualize(C),
  igraph2bigint(C,D).
  
ishow(I):-int2bigint(I,B),bshow(B).
  
bshow(B):-
  write_codes("b="),bigint_print(B),nl,
  bigint2cat(B,C),
  object_to_string(C,S),nl,println(cat=S),
  foreach(morphism_of(C,F,T,_P,_V),
    (bigint_print(F),print_codes("=>"),write(T),put(32))
  ),
  rshow(C),
  true.
  
set_prop_and_color(C,V0,Hyp):-
  % println(here(V0,Hyp)),
  to_string(V0,V),
  set_prop_once(C,V),
  set_hyper(C,V,Hyp).

set_prop_once(C,V):-
  % println(here(V)),
  set_prop_once(C,V,'x','a').

set_prop_once(C,V,X,_):-get_prop(C,V,X,P),P\=='$null',!.
set_prop_once(C,V,X,A):-set_prop(C,V,X,A).

set_morphism_and_color(Cat,A,B):-
  set_morphism_and_color(Cat,A,B,f,t).

set_morphism_and_color(Cat,A,B,K,V):-
  set_morphism_and_color(Cat,A,B,K,V,0).
    
set_morphism_and_color(Cat,A,B,K,V,Hyp):-
  %println(here(A,B)),
  to_string(A,F),to_string(B,T),
  % F=A,B=T,
  %println(there(F,T)),
  set_prop_once(Cat,F),
  set_prop_once(Cat,T),
  set_hyper(Cat,F,Hyp),
  set_hyper(Cat,T,Hyp),
  set_morphism(Cat,F,T,K,V).  
  
  
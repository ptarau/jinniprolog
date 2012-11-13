% converts an int N into a unique hypergraph on  [0..k] 
% where k is the ceiling log of the log of N

int2hg(N,Ess):-
  int2exps(N,Es),
  map(int2exps,Es,Ess).


big2hcat(N,Big,C):-call_java_class_method('jgp.BigTruthTable',big2hcat(N,Big),C).
big2hcat(Big,C):-call_java_class_method('jgp.BigTruthTable',big2hcat(Big),C).

kneval(Max):-
  for(I,0,Max),
  to_bigint(I,B),
  neval(B,R),
  X is I mod 4, % skolem sets X in {0,1}
  R=0,println(I=>R:X),
  fail
; true.


nandtest(NV,Max):-
  for(I,0,Max),
    int2bigint(I,B),
    nandeval(NV,B,NB),
    big2int(NB,NI),
    let(nand_hit,NI,I),
    %bigint_print(B),write(=),bigint_print(NB),nl,
    delete_java_object(B),
    (I\==NI->delete_java_object(NB);true),
  fail
; Ctr=s(0),
  M is (1<<(1<<NV))-1,
  (for(NI,0,M),
     val(nand_hit,NI,_),
     arg(1,Ctr,X),X1 is X+1,change_arg(1,Ctr,X1),
     rm(nand_hit,NI),
     println(hit=NI),
     fail
   ; true
  ),
  arg(1,Ctr,K),
  M1 is M+1,
  println(hits=K/M1).  
   
nandeval(Big,BigR):-call_java_class_method('jgp.BigTruthTable',nandeval(Big),BigR).

nandeval(NVars,Big,BigR):-call_java_class_method('jgp.BigTruthTable',nandeval(NVars,Big),BigR).

neval(Big,R):-call_java_class_method('jgp.BigTruthTable',neval(Big),R).

heval(N,Big,R):-call_java_class_method('jgp.BigTruthTable',heval(N,Big),R).

hcomplexity(Big,R):-call_java_class_method('jgp.BigTruthTable',hcomplexity(Big),R).

gcomplexity(Big,R):-call_java_class_method('jgp.BigTruthTable',gcomplexity(Big),R).

sdistance(F,T,R):-
  call_java_class_method('jgp.BigTruthTable',sdistance(F,T),R).
  
hdistance(F,T,R):-
  call_java_class_method('jgp.BigTruthTable',hdistance(F,T),R).

gdistance(F,T,R):-
  call_java_class_method('jgp.BigTruthTable',gdistance(F,T),R).

hxor(A,B,R):-
  call_java_class_method('jgp.BigTruthTable',xor(A,B),R).


% maps a number to a HFS
% constructively computes a reverse for Ackermann's
% encoding from HFS to positive ints

int2hfs(N,H):-int2exps(N,Es),map2exps(Es,H).

map2exps([],[]).
map2exps([E|Es],[H|Hs]):-int2hfs(E,H),map2exps(Es,Hs).

% maps an int to a list of exponents of 2
int2exps(N,Es):-int2rbits(N,Bs),rbits2exps(Bs,Es).

% maps a list of exponents of 2 to an int
exps2int(Es,N):-exps2int(Es,0,N).

exps2int([],N,N).
exps2int([E|Es],N1,N3):-exp2(E,I),N2 is N1+I,exps2int(Es,N2,N3).


% ints to hfs with functor representation

int2ffs(N,F):-int2exps(N,Es),fmap2exps(Es,H),h2f(H,F).

h2f(H,F):-atomic(H),!,F=o.
h2f(H,F):-F=..[f|H].

fmap2exps([],[]).
fmap2exps([E|Es],[H|Hs]):-int2ffs(E,H),fmap2exps(Es,Hs).

/* generalizations of Ackerman's encoding of 
   hereditarily finite sets as (large) ints, and back
   for the case of N urelements represented as
   (small) ints
*/

% int to tree of 0,1,...MaxUr urelements

int2ur(N,Ur):-int2ur(N,1,Ur).

int2ur(Ur,MaxUr,Ur):-Ur<MaxUr,!.
int2ur(N,MaxUr,UrTree):-
  int2exps(N,Es),
  ints2urs(Es,MaxUr,Us),
  UrTree=..[u|Us].

ints2urs([],_,[]).
ints2urs([E|Es],MaxUr,[U|Us]):-
  int2ur(E,MaxUr,U),
  ints2urs(Es,MaxUr,Us).

% dedekind mapping (a,b)={a,{a,b}} - not a bijection, inefficient
rat2ur(I,I,U):-
  int2ur(I,A),
  U=u(A,u(A)).
rat2ur(I,J,U):-
  int2ur(I,U1),int2ur(J,U2),
  U=u(U1,u(U1,U2)).

int2urpair(N,P):-int2urpair(N,1,P).

int2urpair(N,MaxUr,P):-
  N>=MaxUr,
  int2exps(N,Es),
  member(E,Es),
  ( P=e(N,E)
  ; int2urpair(E,MaxUr,P)
  ).
  
int2urcat(N,MaxUr,C):-
  new_cat(C),
  foreach(
    int2urpair(N,MaxUr,e(A,B)),
    set_morphism_and_color(C,A,B)
  ).
 
% same as int2ur/3 but with Bits indicating MaxUr=2^Bits
exp2ur(N,Bits,Ur):-exp2(Bits,MaxUr),int2ur(N,MaxUr,Ur).

% urelement tree to int

ur2int(Ur,Ur):-integer(Ur),!.
ur2int(UrTree,N):-
  UrTree=..[_|Urs],
  urs2ints(Urs,Es),
  exps2int(Es,N).
/* 
   exps2int can be implemented as a bunch of decoders
   shring the same output which in turn are used as input
   for the next layer of decoders
   if exps range in 0..n-1 then int will range in 0..2^n-1
   
   i.e. in computing u(0,1) exps2int([0,1],R)
   takes 0 as a 1-2 decoder that turns bit 0 on and 1
   as a 1-2 decoder that turns bit 1 on
   
   to compute
   
   u(0,u(0,1)) as 9
   
   we compute u(0,3) as exps2int([0,3],R) with R 2^0+2^3=9
   2^0 and 2^3 are computed by 4-16 decoders
   
   the urelment function u(x0,...,xk) can be seen
   as 2^x0+2^x1+...2^xk
   
   0->0
   u(0)=2^0->1
   
   note that u(...) is symetric and a canonical
   notation puts arguments in order
   
   u(...) < v(...) if for their associated ints n(u)<n(v)
   
   to be canonical the u encoding assumes ordering and
   non-repetition i.e. each level represents sets
*/

urs2ints([],[]).
urs2ints([U|Us],[E|Es]):-
  ur2int(U,E),
  urs2ints(Us,Es).

uplus(U,V,S):-ur2int(U,I),ur2int(V,J),K is I+J,int2ur(K,S).

% computes the value of a urelement seen as a tt
% i.e. a function from 0..n-1 to 0..1 encoded
% as an int from 0..2^n-1

eval_ur(Ur,I,O):-
  ur2int(Ur,N),
  eval_tt(N,I,O).

eval_tt(TT,I,O):-
 int2rbits(TT,Bs),
 nth_member(O,Bs,I).
  
% alternative ur2int

ueval(Ur,R):-integer(Ur),!,R=Ur.
% ueval(Ur,R):-Ur=..[_|Urs],map(ulift,Urs,Rs),sum(Rs,R).
ueval(Ur,R):-Ur=..[_|Urs],uevals(Urs,0,R).

ulift(Ur,R):-ueval(Ur,E),exp2(E,R). % idecoder

uevals([],R,R).
uevals([Ur|Urs],R1,R3):-
  ulift(Ur,R),
  R2 is R1+R,
  uevals(Urs,R2,R3).

eplus(A,B,C):-C is A+(B<<1).
bs2i(Bs,N):-foldr(eplus,0,Bs,N).

% decodes bits - Is read as a binary number right to left i.e. 2^0+2^1+...
bdecoder(Is,Os):-rbits2int(Is,I),length(Is,L),exp2(L,LL),length(Os,LL),bind_unary(Os,I,0,_).

bind_unary([],_,N,N).
bind_unary([B|Bs],I,N1,N2):-N is N1+1,bind_one_bit(I,N1,B),bind_unary(Bs,I,N,N2).
  
bind_one_bit(N,N,1).
bind_one_bit(N1,N2,0):-N1=\=N2.

idecoder(I,O):-exp2(I,O). % O is 1<<I 

bdecoder1(Is,Os):-rbits2int(Is,I),idecoder(I,O),int2rbits(O,Os).

% bitstring vars

bits2vurs(NBits:N,Int2x,Vs:Fs):-
  Max is 1<<(1<<NBits),
  N<Max,N>=0,
  !,
  vars_to_bitstrings(Vs,NBits),
  findall(F,(member(V,Vs),call(Int2x,V,F)),Fs).
bits2vurs(NBits:N,Int2x,_):-
  errmes(out_of_range_in(Int2x),NBits:N).

uvertices(Es,Us):-
  findall(V,(member(e(A,B),Es),(V=A;V=B)),Vs),
  sort(Vs,Us).
  
int2vurcat(NBits:N,Cat):-gen2cat(NBits:N,int2urpair,Cat).

gen2cat(NBits:N,Int2x,Cat):-
  new_cat(Cat),
  G=..[Int2x,N,E],
  findall(E,G,Es),uvertices(Es,Us),
  bits2vurs(NBits:N,Int2x,Is:Fs),uvertices(Fs,Vs),
  set_prop_and_color(Cat,N,1),
  foreach(member(I,Is),set_prop_and_color(Cat,I,2)),
  foreach(member(U,Us),set_prop_and_color(Cat,U,0)),
  foreach(member(V,Vs),set_prop_and_color(Cat,V,3)),
  append(Es,Fs,EFs),
  foreach(member(e(F,T),EFs),set_morphism_and_color(Cat,F,T)).


     
% end

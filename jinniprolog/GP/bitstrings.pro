% bits and tt predicates

% FROM ints TO bits

nint2bss(L,N,Yss):-
  exp2(L,Big),
  int2lexps(Big,N,Es),
  map(int2lbits(L),Es,Yss).

% N-th power of 2
%exp2(N,L):-pow(2,N,R),integer(R,L).
exp2(N,L):-L is 1<<N.

ilog2n(N2,N):-N is 1+integer(log(2,N2)). 

int2lexps(NbOfBits,N,Es):-int2lbits(NbOfBits,N,Bs),rbits2exps(Bs,Es).

% converts an int to a list of bits 
int2bits(N,Bs):-integer(N),int2rbits(N,Rs),reverse(Rs,Bs).

int2lbits(NbOfBits,N,Bs):-int2bits(N,As),lpad_bits(NbOfBits,As,Bs).

lpad_bits(N,Bs,NewBs):-
  %N is 1<<NbOfBits,
  length(Bs,L),
  K is N-L,
  lpad_expand(K,Bs,NewBs).

lpad_expand(0,Bs,Bs).
lpad_expand(K,Bs,[0|NewBs]):-
  K>0,
  K1 is K-1,
  lpad_expand(K1,Bs,NewBs).
 
int2rbits(0,[]).
int2rbits(N,[B|Bs]):-N>0,B is N mod 2, N1 is N//2,int2rbits(N1,Bs).

rpad_bits_to(Max,Bs,NewBs):-rpad_bits_to(Max,Bs,NewBs,[]).

rpad_bits_to(0,[])-->[].
rpad_bits_to(Max,Bs)-->{Max>0,Max1 is Max-1},expand_bits(Bs,NewBs),rpad_bits_to(Max1,NewBs).

expand_bits([],[])-->[0].
expand_bits([B|Bs],Bs)-->[B].

% FROM bits TO ints

% converts a list of bits into an int
lbits2int(Bs,N):-nonvar(Bs),reverse(Bs,Rs),rbits2int(Rs,N).

rbits2int(Rs,N):-nonvar(Rs),rbits2int(Rs,0,0,N).

rbits2int([],_,N,N).
rbits2int([X|Xs],E,N1,N3):-
  NewE is E+1,
  N2 is X<<E+N1,
  rbits2int(Xs,NewE,N2,N3).

% maps reversed bits to exponents of 2 ONSET only
rbits2exps(Bs,Es):-rbits2exps(1,Bs,Es).

rbits2exps(Bit,Bs,Es):-rbits2exps(Bs,Bit,0, Es).

rbits2exps([],_,_,[]).
rbits2exps([B|Bs],Bit,E,NewEs):-
  E1 is E+1,
  rbit2exp(Bit,B,E,NewEs,Es),
  rbits2exps(Bs,Bit,E1,Es).

rbit2exp(Bit,B,_,Es,Es):-B=\=Bit.
rbit2exp(Bit,B,E,[E|Es],Es):-B=:=Bit.
        
% maps bits to elements of a set they select

rbits_mask([],[],[],[]).
rbits_mask([B|Bs],[E|Es],NewNs,NewYs):-
  rbit_mask(B,E,NewNs,Ns,NewYs,Ys),
  rbits_mask(Bs,Es,Ns,Ys).

rbit_mask(0,N,[N|Ns],Ns,Ys,Ys).
rbit_mask(1,Y,Ns,Ns,[Y|Ys],Ys).

% apply a tt as function to an int seen as a variable assignment

tt_apply(TT,V0_N,R):-int2exps(TT,Es),(member(V0_N,Es)->R=1;R=0).

% circuit eval algo: decompose V0_N in "parts" and see if its parts "fire" TT


/*
  Unbilievable but true - we are is slightly faster than C based SWI-Prolog!
  in this test.
*/

app([],Ys,Ys).
app([A|Xs],Ys,[A|Zs]):-
  app(Xs,Ys,Zs).

nrev([],[]).
nrev([X|Xs],R):-
  nrev(Xs,T),
  app(T,[X],R).

range(Min,Min,Max):-Min=<Max.
range(I,Min,Max):-
        Min<Max,
        Min1 is Min+1,
        range(I,Min1,Max).

integers([],I,I):-!.
integers([I0|L],I0,I):-I0<I,I1 is I0+1,integers(L,I1,I).

empty_for(It):-range(_,1,It),true,fail.
empty_for(_).

full_for(It,L):-range(_,1,It),nrev(L,_),fail.
full_for(_,_).

bm(It,Len,Time,Lips):-
	integers(L,0,Len),
	statistics(cputime,T0),
	empty_for(It),
	statistics(cputime,T1),
	full_for(It,L),
	statistics(cputime,T2),
	Time is (T2-T1)-(T1-T0),
	L1 is Len+1,
	L2 is Len+2,
	P is L1*L2,
	LI is P//2,
	LIs is It*LI,
	Lips is integer(LIs/Time). 

htest(N,H,T,S):-
  integers(Is,0,N),
  statistics(heapused,H1),
  statistics(trailused,T1),
  statistics(localused,S1),
  nrev(Is,_),
  statistics(heapused,H2),
  statistics(trailused,T2),
  statistics(localused,S2),
  H is H2-H1,T is T2-T1,S is S2-S1.

rtest(I,N,[iterations,I,size,N,time,T,lips,L,heap,H,trail,Tr,stack,S]):-
  bm(I,N,T,L),
  htest(N,H,Tr,S).

t(X):-rtest(200,100,X).

go:-t(X),write(X),nl.

sgo:-rtest(10,30,X),write(X),nl.


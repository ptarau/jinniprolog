app([],Ys,Ys).
app([A|Xs],Ys,[A|Zs]):-
  app(Xs,Ys,Zs).

nrev([],[]).
nrev([X|Xs],Zs):-
  nrev(Xs,Ys),
  app(Ys,[X],Zs).

fnrev([],[]).
fnrev([X|Xs],Zs):-
  fnrev(Xs,Ys),
  det_append(Ys,[X],Zs).

full_range(It,L):- range(_,1,It),nrev(L,_),fail.
full_range(_,_).

dummy(_,_).

empty_range(It,L):-range(_,1,It),dummy(L,_),fail.
empty_range(_,_).

range(Min,Min,Max):-Min=<Max.
range(I,Min,Max):-
        Min<Max,
        Min1 is Min+1,
        range(I,Min1,Max).

integers([],I,I):-!.
integers([I0|L],I0,I):-I0<I,I1 is I0+1,integers(L,I1,I).

lgo1(R):-nrev([1,2,3],R).

lgo(nrev(iter(It),len(Len),time(T))):-
  Len=100,
	It=3000,
  tbm(It,Len,T).

tbm(It,Len,MsTime):-
	integers(L,0,Len),
	timer(T0),
	empty_range(It,L),
	timer(T1),
	full_range(It,L),
	timer(T2),
	MsTime is (T2-T1)-(T1-T0).

bm(It,Len,Time,Lips):-
  tbm(It,Len,MsTime),
  Time is MsTime/1000.0,
	L1 is Len+1,
	L2 is Len+2,
	LI is (L1*L2)/2,
	LIs is It*LI,
	Lips is (LIs/Time)/1000.0.

htest(N,H,T,S):-
        integers(Is,0,N),
        statistics(global_stack,[H1,_]),
        statistics(trail,[T1,_]),
        statistics(local_stack,[S1,_]),
        nrev(Is,_),
        statistics(global_stack,[H2,_]),
        statistics(trail,[T2,_]),
        statistics(local_stack,[S2,_]),
        H is H2-H1,
        T is T2-T1,
        S is S2-S1.

go3(Mes,Len,It):-
	bm(It,Len,T,L),
	htest(Len,H,Tr,S),
	nl,write(Mes=[klips=L,time=T,length=Len,iterations=It,
                      heap=H,trail=Tr,stack=S]),nl.

gg:-go2(50,100).

g0:-go2(100,200).

g1:-go2(100,800).

g2:-go2(200,200).

g3:-call(write('use bp -h20000')),nl,go2(2000,1).

g4:-
  N=2000,
  integers(Xs,0,N),nrev(Xs,Ys),length(Ys,N),statistics,write(length=N+Ys),nl.

go2(L,I):-go3(test,L,I).

go:-go1('BMARK_brev:').

go1(Mes):-
	Len=100,
	It=300,
	go3(Mes,Len,It).

big:-big1('BMARK_brev_100_times_3000_elem_nrev:').

big1(Mes):-
	Len=100,
	It=3000,
	go3(Mes,Len,It).

small:-small1('BMARK_brev_100_times_30_elem_nrev:').

small1(Mes):-
	Len=30,
	It=100,
	go3(Mes,Len,It).
	
timer(T0):-statistics(runtime,[T0,_]).
%timer(T):-statistics(cputime,T).
%timer(T):-T is cputime.


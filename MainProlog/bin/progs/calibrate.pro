% parrallel, nrev only

pargo:-pargo(8).

pargo(Tasks):-
  parenrev(Tasks,200,400,Result),
  println(Result).

% sequential blen
go:-
  calibrate(T,Times),
  write(T=Times),nl.

calibrate(T):-calibrate(T,_).

calibrate(T,Details):-calibrate(30,T,Details).
  
calibrate(Multiplier,T,times(infs(LI),lips(LIPS),nrev(TN),perm(TP),assert(TA))):-
   Base is 5,
   NTimes is Base*Multiplier,
   PTimes is Base*Multiplier,
   ATimes is Base*Multiplier,
   Len is 200,
   nrevtest(Len,NTimes,LI,LIPS,TN),
   getctime(T1),
   permtest(7,PTimes),
   getctime(T2),
   asserttest(5,ATimes),
   getctime(T3),
   TP is T2-T1,
   TA is T3-T2,
   T is TN+TP+TA.
   
fortime(Len,Times,T,Is):-getctime(TB),fortest(Times),getctime(TE),T is TE-TB,findall(I,'$for'(I,1,Len),Is).

fortest(N):-'$for'(_I,1,N),fail.
fortest(_).

nrevtest(Len,NTimes,LI,LIPS,TN):-
  fortime(Len,NTimes,TF,Is),
  getctime(T0),
  nrevtimes(Is,NTimes),
  getctime(T1),
  TN is T1-T0,
  LI is (NTimes/1.0001)*(((Len+1)*(Len+2))//2),
  TS is (TN-TF)/1000.0001,
  LIPS is integer(LI/TS).
  
nrevtimes(Is,Times):-
  '$for'(_I,1,Times),
  '$nrev'(Is,_),
  fail.
nrevtimes(_,_).

'$app'([],Ys,Ys).
'$app'([A|Xs],Ys,[A|Zs]):-
  '$app'(Xs,Ys,Zs).

'$nrev'([],[]).
'$nrev'([X|Xs],Zs):-
  '$nrev'(Xs,Ys),
  '$app'(Ys,[X],Zs).

permtest(N,Times):-
  '$for'(_I,1,Times),
  permtest(N),
  fail.
permtest(_N,_Times).
  
permtest(N):-
  findall(I,'$for'(I,1,N),Is),
  '$perm'(Is,_),
  fail.
permtest(_).

'$perm'([],[]).
'$perm'([X|Xs],Zs):-
	'$perm'(Xs,Ys),
	'$insert'(X,Ys,Zs).

'$insert'(X,Ys,[X|Ys]).
'$insert'(X,[Y|Ys],[Y|Zs]):-
	'$insert'(X,Ys,Zs).

asserttest(MX,MY):-
  initboard1(MX,MY),
  updateboard1(MX,MY),
  accessboard1(MX,MY),
  cleanboard1.

initboard1(MaxX,MaxY):-
	'$for'(X,1,MaxX),
	'$for'(Y,1,MaxY),
	assert('$board'(X,Y,0)),
	fail.
initboard1(_,_).

updateboard1(MaxX,MaxY):-
	'$for'(X,1,MaxX),
	'$for'(Y,1,MaxY),
        update1(X,Y),
	fail.
updateboard1(_,_).
	
update1(X,Y):-
  retract('$board'(X,Y,_)),
  !,
  assert('$board'(X,Y,1)).

accessboard1(MaxX,MaxY):-
	'$for'(X,1,MaxX),
	'$for'(Y,1,MaxY),
 	'$board'(X,Y,_),
	fail.
accessboard1(_,_).

cleanboard1:-
  abolish('$board'/3).

getctime(T):-statistics(runtime,[T,_]).

'$for'(Min,Min,Max):-Min=<Max.
'$for'(I,Min,Max):-
  Min<Max,
  Min1 is Min+1,
  '$for'(I,Min1,Max).

parenrev(Tasks,Len,Times, [lips(PLIPS),time(T),times(Ts)]):-
  G=nrevtest(Len,Times,_LI, _LIPS,_TN),
  findall(G,'$for'(_,1,Tasks),Gs),
  getctime(T1),
  mbg(Gs),
  getctime(T2),
  map(arg(3),Gs,Ls),sum(Ls,L),
  map(arg(5),Gs,Ts),
  T is T2-T1,
  PLIPS is integer(L/(T/(1000.0001))).
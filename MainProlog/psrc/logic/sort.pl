/*
% sort, adapted from public domain code written by R.A. O'Keefe
% use merge_sort(<,_,_) if you do not want duplications eliminated
% use merge_sort(>,_,_) for descending order
*/

sort(L1,L2):-merge_sort(<,L1,DupL),remdup(DupL,L2).

keysort(L,S):-ksort(L,S).
      
merge_sort(Rel, L,S ):-
	length(L,N),
	merge_sort1(N, Rel, L,S,[] ).

merge_sort1( 0,_,L,[],L ):-!.
merge_sort1( 1,_,[X|L],[X],L ):-!.
merge_sort1( N,Rel,L,S,R ):-				% N >= 2
	N1 is N >> 1,	N2 is N-N1,
	merge_sort1( N1,Rel,L,S1,R1),	
	merge_sort1( N2,Rel,R1,S2,R),
	merge_2( S2,Rel,S1,S ).

merge_2([],_,S,S ):-!.
merge_2([X|L1],Rel,[Y|L2],[X|L] ):-compare(Rel,X,Y),!,
	merge_2(L1,Rel,[Y|L2],L ).
merge_2(L1,Rel,[Y|L2],[Y|L] ):-
	merge_2(L2,Rel,L1,L ).

ksort(List, Sorted) :-
	keysort(List, -1, S, []), !,
	Sorted = S.
ksort(X, Y):-user_error('illegal_arguments',keysort(X,Y)).

keygroup(KsVs,K,Vs):-
  keysort(KsVs,Sorted),
  concordant_subset(Sorted,K,Vs).
  
keysort([Head|Tail], Lim, Sorted, Rest) :- !,
	nonvar(Head),
	Head = _-_,
	Qh = [Head|_],
	samkeyrun(Tail, Qh, Qh, Run, Rest0),
	keysort(Rest0, 1, Lim, Run, Sorted, Rest).
keysort(Rest, _, [], Rest).

keysort([Head|Tail], J, Lim, Run0, Sorted, Rest) :-
	J =\= Lim, !,
	nonvar(Head),
	Head = _-_,
	Qh = [Head|_],
	samkeyrun(Tail, Qh, Qh, Run1, Rest0),
	keysort(Rest0, 1, J, Run1, Run2, Rest1),
	keymerge(Run0, Run2, Run),
	K is J+J,
	keysort(Rest1, K, Lim, Run, Sorted, Rest).
keysort(Rest, _, _, Sorted, Sorted, Rest).

samkeyrun([Hd|Tail], QH, QT, Run, Rest) :-
	nonvar(Hd),
	Hd = H-_,
	QT = [Q-_|QT2], 
	Q @=< H, !,
	QT2 = [Hd|_],
	samkeyrun(Tail, QH, QT2, Run, Rest).
samkeyrun([Hd|Tail], QH, QT, Run, Rest) :-
	nonvar(Hd),
	Hd = H-_,
	QH = [Q-_|_],
	H @< Q, !,
	samkeyrun(Tail, [Hd|QH], QT, Run, Rest).
samkeyrun(Rest, Run, [_], Run, Rest).

% keymerge(+List, +List, -List).
keymerge([], L2, Out) :- !,
	Out = L2.
keymerge([H1|T1], L2, Out) :-	
	L2 = [K2-_|_],
	H1 = K1-_,
	K1 @=< K2, !,
	Out = [H1|Out1],
	keymerge(T1, L2, Out1).
keymerge(L1, [H2|L2], Out) :- !,
	Out = [H2|Out1],
	keymerge(L1, L2, Out1).
keymerge(List, _, List).

% removes duplicates

remdup([],[]).
remdup([X,Y|Xs],Ys):-compare(=,X,Y),!,remdup([X|Xs],Ys).
remdup([X|Xs],[X|Ys]):-remdup(Xs,Ys).

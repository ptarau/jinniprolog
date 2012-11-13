% DCGs

expand_term(C,E):-portable_expand_term(C,E).
std_expand_term(C,D):-portable_expand_term(C,D).

%   File   : DCG.PL Author : Richard A. OKeefe

dcg_rule('-->'(Head0,Body0), Clause) :-
	dcg_head(Head0, Head, PushBack, S0, S),
	dcg_body(Body0, Body1, S0, S),
	dcg_conj(Body1, PushBack, Body),
	Clause = ':-'(Head,Body).

dcg_head((Head0,PushBack0), Head, PushBack, S0, S1) :- !,
	dcg_goal(Head0, Head, S0, S),
	dcg_body(PushBack0, PushBack, S, S1).
dcg_head((A0;B0), (A;B), true, S0, S) :- !,
	dcg_disj(A0, A, S0, S),
	dcg_disj(B0, B, S0, S).
dcg_head(Head0, Head, true, S0, S) :-
	dcg_goal(Head0, Head, S0, S).

dcg_goal(Goal0, Goal, S0, S) :-
	functor(Goal0, F, N),
	N1 is N+1,
	N2 is N+2,
	functor(Goal, F, N2),
	arg(N2, Goal, S),
	arg(N1, Goal, S0),
	dcg_args(N, Goal0, Goal).

dcg_args(N, Goal0, Goal) :-
	(   N =:= 0 -> true
	;   arg(N, Goal0, Arg),
	    arg(N, Goal,  Arg),
	    M is N-1,
	    dcg_args(M, Goal0, Goal)
	).

dcg_body(Var, Body, S0, S) :- var(Var), !,
	Body = phrase(Var,S0,S).
dcg_body((A0,B0), Body, S0, S) :- !,
	dcg_body(A0, A, S0, S1),
	dcg_body(B0, B, S1, S),
	dcg_conj(A, B, Body).
dcg_body((A0->B0), (A->B), S0, S) :- !,
	dcg_body(A0, A, S0, S1),
	dcg_body(B0, B, S1, S).
dcg_body((A0;B0), (A;B), S0, S) :- !,
	dcg_disj(A0, A, S0, S),
	dcg_disj(B0, B, S0, S).
dcg_body({A}, A, S, S) :- !.
dcg_body('#'(A), '#:'(A,S1,S2), S1, S2) :- !.
dcg_body(!, !, S, S) :- !.
dcg_body([], true, S, S) :- !.
dcg_body([H0|T0], Body, S0, S) :- !,
	dcg_term(H0, H, S0, S1),
	dcg_body(T0, T, S1, S),
	dcg_conj(H, T, Body).
dcg_body(NT0, NT, S0, S) :-
	dcg_goal(NT0, NT, S0, S).

dcg_term(T0, 'C'(S0,T0,S), S0, S).

dcg_disj(Body0, Body, S0, S) :-
	dcg_body(Body0, Body1, S1, S),
	(   S1==S -> dcg_conj(S1=S0, Body1, Body)
	;   S1 = S0, Body = Body1
	).

dcg_conj(A, true, A) :- !.
dcg_conj(A, B, C) :-
	dcg_CONJ(A, B, C).

dcg_CONJ(true, C, C) :- !.
dcg_CONJ((A,As), C0, (A,C)) :- !,
	dcg_CONJ(As, C0, C).
dcg_CONJ(A, C, (A,C)).

'C'([T|Ts], T, Ts).

phrase(NT0, S0) :-
	phrase(NT0, S0, []).

phrase(NT0, S0, S) :-
	dcg_body(NT0, NT, T0, T),
	T0 = S0, T = S,
	NT.

portable_expand_term('-->'(H,B),Clause):-!,
	(	dcg_rule('-->'(H,B),Clause)->true
	;	write('dcg_expansion_error->'),write(H),nl,fail
	).
portable_expand_term(C,C).


% DCG alternative to AG stuff

set_dcg_state(Xs,_,Xs).

get_dcg_state(Xs,Xs,Xs).

dcg_look_ahead(Next,[Next|Xs],[Next|Xs]).

dcg_look_ahead(First,Second,[First,Second|Xs],[First,Second|Xs]).


'@'(X)-->[X].

match_word([C|Cs])--> [C],!,match_word(Cs).
match_word([])-->[].

match_before(Stop,Cs)-->match_before([Stop],Cs,_).

match_before(Stops,[],Stop)-->[Stop],{member(Stop,Stops)},!.
match_before(Stops,[C|Cs],Stop)-->[C],match_before(Stops,Cs,Stop).


% regexp tools with  AGs + high order

one(F,[X])--> dcg_call(F,X).

star(F,[X|Xs])--> dcg_call(F,X),!,star(F,Xs).
star(_,[])-->[].

plus(F,[X|Xs])--> dcg_call(F,X),star(F,Xs).

dcg_call(F,X,D,S1,S2):-FX=..[F,X,D,S1,S2],topcall(FX). %,println(called=FX).

dcg_call(F,X,S1,S2):-FX=..[F,X,S1,S2],topcall(FX). %,println(called=FX).

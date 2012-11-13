/* R.A. O'Keefe's meta-circular interpreter (see also extra.pl) */

metacall(Body):-do_body(Body).

/* % was buggy - if  then else backtrack - thanks Doug !!!
do_body((!,AfterCut), AfterCut, yes) :- !.
do_body((Goal,Body), AfterCut, HadCut) :- !,
	do_goal(Goal),
	do_body(Body, AfterCut, HadCut).
do_body(!, true, yes).
do_body((Disj1;_), AfterCut, HadCut) :-
	do_body(Disj1, AfterCut, HadCut).
do_body((_;Disj2), AfterCut, HadCut) :- !,
	do_body(Disj2, AfterCut, HadCut).
do_body(Goal, true, no) :-
	do_goal(Goal).
*/

do_body(Body):-var(Body),!,errmes(unbound_goal,var(Body)).
do_body(Body) :-
	do_body(Body, AfterCut, HadCut),
	( HadCut = yes,
		!,
		do_body(AfterCut)
	;   HadCut = no
	).

do_body((Goal,Body), AfterCut, HadCut):-!,
	do_conj(Goal,Body, AfterCut, HadCut).
do_body((Goal;Body), AfterCut, HadCut):-!,
	do_disj(Goal,Body, AfterCut, HadCut).
do_body(!, true, yes):-!.
do_body(Goal, true, no) :- do_goal(Goal).

do_conj(!,AfterCut, AfterCut, yes) :- !.
do_conj(Goal,Body, AfterCut, HadCut) :- 
	do_body(Goal),
	do_body(Body, AfterCut, HadCut).

do_disj((If->Then),Else, AfterCut, HadCut) :-!,
   do_if_then_else(If,Then,Else, AfterCut, HadCut).
do_disj(Disj1,Disj2, AfterCut, HadCut) :-
   do_disj0(Disj1,Disj2, AfterCut, HadCut).

do_if_then_else(If,Then,_, AfterCut, HadCut) :- do_body(If),!,
	do_body(Then, AfterCut, HadCut).
do_if_then_else(_,_,Else, AfterCut, HadCut) :-
	do_body(Else, AfterCut, HadCut).

do_disj0(Disj1,_, AfterCut, HadCut) :-
	do_body(Disj1, AfterCut, HadCut).
do_disj0(_,Disj2, AfterCut, HadCut) :-
	do_body(Disj2, AfterCut, HadCut).





do_goal(Goal) :-
	is_compiled(Goal), % <--- check for a compiled predicate
	!,
	%println('calling compiled'(Goal)),
	Goal.
do_goal(Goal) :- % in fact, this can be safely left to the interpreter once it handles ! and calls back the compiler
	is_dynamic_or_debugged(Goal),
    !,
	clause(Goal, Body),	% <--- assume anything else is interpreted
	do_body(Body, AfterCut, HadCut),
	(	HadCut = yes,
		!,
		do_body(AfterCut)
	;	HadCut = no
	).
do_goal(Undef):-
  delegate_undefined(Undef).
  
delegate_undefined(Undef):-is_compiled(on_undefined(_)),!,call(on_undefined(Undef)).
delegate_undefined(Undef):-is_dynamic_or_debugged(on_undefined(_)),!,do_goal(on_undefined(Undef)).
delegate_undefined(Undef):-errmes(undefined_predicate_in_metacall,Undef).

% on_undefined(_):-fail.

is_dynamic_or_debugged(H):-is_dynamic(H),!.
is_dynamic_or_debugged(H):-call_ifdef(debug_clause(H,_),fail),!. % cuts multiple answers here

X is E:-meta_is(E,R),!,X=R.
X is E:-errmes(error_in_is,X is E).

meta_is(E,R):-number(E),!,R=E.
meta_is(E,R):-atomic(E),!,R=E.
meta_is(E,_Error):-var(E),!,throw(arithmetic_error(unbound,E)).
meta_is('+'(X),R):-!,meta_is(X,R).
meta_is('-'(X),R):-!,meta_is(X,N),'-'(0,N,R).
meta_is(\(X),R):-!,meta_is(X,N),\(N,R).
meta_is(compute(Op,E1),R):-!,
	meta_is(E1,X1),
	compute(Op,X1,R).
meta_is(compute(Op,E1,E2),R):-!,
	meta_is(E1,X1),
	meta_is(E2,X2),
	compute(Op,X1,X2,R).
meta_is(E,R):-compound(E),E=..[Op,E1,E2],!,
	meta_is(E1,X1),
	meta_is(E2,X2),
	G=..[Op,X1,X2,R],
	G.
meta_is(E,R):-compound(E),E=..[Op,E1],!,
	meta_is(E1,X1),
	G=..[Op,X1,R],
	G.
	
'?'(G):-
  S=s(0),
  pp_clause(calling(G)),
  if_any(G,trace_success(S,G),trace_failure(G)).

trace_success(S,G):-arg(1,S,K),K1 is K+1,change_arg(1,S,K1),pp_clause(exiting(K1,G)).

trace_failure(G):-pp_clause(failing(G)),fail. 

%   Based on file   : INTERP
%   Author : R.A.O'Keefe
%   Updated: 2 March 84
%   Purpose: Meta-circular interpreter for Prolog

/*  This is a genuinely meta-circular interpreter for a subset of Prolog
    containing cuts.  It relies on the fact that disjunction is transparent
    to cut just like conjunction.  If it doesn't work in your Prolog, and
    if you paid more than $100 for it, take your Prolog back to the shop
    and insist that they fix it, there are at least four different ways of
    implementing disjunction so that it works.
*/

% the metainterpreter moved to top.pl => Paul Tarau

debug(Goal) :-
	qdebug(Goal).
	
trace(Goal) :-
	tr_goal(Goal, 0).


tr_goal(call(Goal), Depth) :- !,
	nonvar(Goal),
	tr_body(Goal, Depth).
tr_goal(\+(Goal), Depth) :-
	tr_body(Goal, Depth),
	!, fail.
tr_goal(\+(_), _) :- !.
tr_goal(Goal, Depth) :-
	(   tab(Depth), write('Call: '), print(Goal), nl, fail
	;   Depth1 is 1+Depth,
	    tr_call(Goal, Depth1),
	    (   tab(Depth), write('Exit: '), print(Goal), nl, fail
	    ;	true
	    ;   tab(Depth), write('Redo: '), print(Goal), nl, fail
	    )
	;   tab(Depth), write('Fail: '), print(Goal), nl, fail
	).

tr_call(bagof(X,Y,Z), Depth) :- !,		% add these 4 lines if you
	bagof(X, tr_body(Y,Depth), Z).		% really want them, but they do
tr_call(setof(X,Y,Z), Depth) :- !,		% slow things down a bit.
	setof(X, tr_body(Y,Depth), Z).
tr_call(findall(X,Y,Z), Depth) :- !,
	findall(X, tr_body(Y,Depth), Z).
tr_call(Goal, _) :-
	is_compiled(Goal),		% <--- check for a compiled predicate
	!,
	call(Goal).
tr_call(Goal, Depth) :-
	is_dynamic_or_debugged(Goal),
	!,
	clause(Goal,Body),
	tr_body(Body, Depth, AfterCut, HadCut),
	(   HadCut = yes,
		!,
		tab(Depth), write('CUT'), nl,
		tr_body(AfterCut, Depth)
	;   HadCut = no
	).
tr_call(Goal, _Depth) :- 
    /* likely to be undefined or delegated - let
       metacall handle it - we reuse here its behavior     
    */
    metacall(Goal).
    
tr_body(Body, Depth) :-
	tr_body(Body, Depth, AfterCut, HadCut),
	(   HadCut = yes,
		!,
		tab(Depth), write('CUT'), nl,
		tr_body(AfterCut, Depth)
	;   HadCut = no
	).

tr_body((Conj1,Conj2), Depth, AfterCut, HadCut) :- !,
	tr_body(Conj1, Conj2, Depth, AfterCut, HadCut).
tr_body(!, _, true, yes) :- !.
tr_body((Disj1;_), Depth, AfterCut, HadCut) :-
	tr_body(Disj1, Depth, AfterCut, HadCut).
tr_body((_;Disj2), Depth, AfterCut, HadCut) :- !,
	tr_body(Disj2, Depth, AfterCut, HadCut).
tr_body(true, _, true, no) :- !.
tr_body(Goal, Depth, true, no) :-
	tr_goal(Goal, Depth).

tr_body(!, AfterCut, _, AfterCut, yes) :- !.
tr_body((A,B), Conj, Depth, AfterCut, HadCut) :- !,
	tr_body(A, (B,Conj), Depth, AfterCut, HadCut).
tr_body((Disj1;_), Conj, Depth, AfterCut, HadCut) :-
	tr_body(Disj1, Conj, Depth, AfterCut, HadCut).
tr_body((_;Disj2), Conj, Depth, AfterCut, HadCut) :- !,
	tr_body(Disj2, Conj, Depth, AfterCut, HadCut).
tr_body(true, Body, Depth, AfterCut, HadCut) :- !,
	tr_body(Body, Depth, AfterCut, HadCut).
tr_body(Goal, Body, Depth, AfterCut, HadCut) :-
	tr_goal(Goal, Depth),
	tr_body(Body, Depth, AfterCut, HadCut).


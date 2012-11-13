
% data type checking and conversions

atom(X):-integer(X),!,fail.
atom(X):-atomic(X).

number(X):-integer(X),!.
number(X):-var(X),!,fail.
number('$float'(_,_,_)).

float(X):-var(X),!,fail.
float('$float'(_,_,_)).

compound(X):-nonvar(X), \+(atomic(X)).

atom_codes(A,Cs):-name(A,Cs,fail).

name(A,Cs):-name(A,Cs,'$null'). % force conversion to number, if possible

number_codes(A,Cs):-name(A,Cs,true).

atom_chars(A,Cs):-nonvar(A),!,atom_codes(A,Xs),map(code_char,Xs,Cs).
atom_chars(A,Cs):-nonvar(Cs),map(char_code,Cs,Xs),atom_codes(A,Xs).

number_chars(A,Cs):-nonvar(A),!,number_codes(A,Xs),map(code_char,Xs,Cs).
number_chars(A,Cs):-nonvar(Cs),map(char_code,Cs,Xs),number_codes(A,Xs).

code_char(Code,Char):-atom_codes(Char,[Code]).

char_code(Char,Code):-atom_codes(Char,[Code]).

is_functor(FN):-nonvar(FN),FN=(F/N),atom(F),integer(N),N>=0,N=<255.

'=..'(T,[F|Xs]):-nonvar(T),!,functor(T,F,N),term2list(1,N,T,Xs).
'=..'(T,[F|Xs]):-get_length(Xs,0,N),!,functor(T,F,N),term2list(1,N,T,Xs).

term2list(I,N,_,R):-I>N,!,R=[].
term2list(I,N,T,[X|Xs]):-I1 is I+1,arg(I,T,X),term2list(I1,N,T,Xs).

term_append(Closure,Args,Goal):-
	'=..'(Closure,FXs),
	'=..'(Args,[_|Ys]),
	det_append(FXs,Ys,FXsYs),
	'=..'(Goal,FXsYs).

intialization(X):-topcall(X).

numbervars('$VAR'(N0), N0, N) :- !, N is N0+1.
numbervars(X, N0, N) :- atomic(X), !, N0=N.
numbervars([X|Xs], N0, N) :- !,
  numbervars(X, N0, N1),
  numbervars(Xs, N1, N).
numbervars(X, N0, N) :-
  functor(X, _, A),
  numbervars(0, A, X, N0, N).

numbervars(A, A, _, N0, N) :- !, N0=N.
numbervars(A0, A, X, N0, N) :-
  A1 is A0+1,
  arg(A1, X, X1),
  numbervars(X1, N0, N1),
  numbervars(A1, A, X, N1, N).


statistics(runtime,[Last,Now]):-runtime(Last,Now).
statistics(global_stack,[Used,Free]):-global_stack(Used,Free).
statistics(local_stack,[Used,Free]):-local_stack(Used,Free).
statistics(trail,[Used,Free]):-trail(Used,Free).
statistics(code,[Used,Free]):-code(Used,Free).
statistics(symbols,[Used,Free]):-symbols(Used,Free).
statistics(htable,[Used,Free]):-htable(Used,Free).
%statistics(bboard,[Used,Free]):-bboard(Used,Free).
%statistics(strings,[Used,Free]):-strings(Used,Free).
  
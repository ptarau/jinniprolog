pp(C):-pp_clause(C).

pp_clause(C):-portray_clause(C).

portray_clause(C):-numbervars(C,0,_),pp_clause0(C),fail.
portray_clause(_).

pp_clause0(C):-var(C),!,top_writeq_atom(C).
pp_clause0(:-(Body)) :- !,
        nl,
        l_clauses(Body, 0, 2, 8).
pp_clause0((Pred:-Body)) :-
        top_writeq_atom(Pred),
        l_clauses(Body, 0, 2, 8), !.
pp_clause0((Pred)) :-
        pp_clause0((Pred:-true)).

l_clauses(C, _L, _R, _D):-var(C),!,top_writeq_atom(C).
l_clauses((A,B), L, R, D) :- !,
        l_clauses(A, L, 1, D), !,
        l_clauses(B, 1, R, D).
l_clauses(true, _, 2, _) :- !,[P]=".",
        put_code(P), nl.
l_clauses((A;B), L, R, D) :- !,
        l_magic(fail, L, D),
        l_magic((A;B), 0, 2, D),
        l_magic_nl(R, '.').
l_clauses((A->B), L, R, D) :- !,
        l_clauses(A, L, 5, D), !,
        l_clauses(B, 5, R, D).
l_clauses(Goal, L, R, D) :-
        l_magic(Goal, L, D),
        top_writeq_atom(Goal),
        l_magic_nl(R,'.').

l_magic(!,    0, _) :- !,pp_write(' :- ').
l_magic(!,    1, _) :- !,pp_write(',  ').
l_magic(_, 0, D) :- !,
        pp_write(' :- '),
        nl, tab(D).
l_magic(_Goal, 1, D) :- !,
        [Char]=",",
        put_code(Char),
        nl, tab(D).
l_magic(_, 3, _) :- !,pp_write('(   ').
l_magic(_, 4, _) :- !,
        pp_write(';   ').
l_magic(_, 5, D) :- !,
        pp_write(' ->'),
        nl, tab(D).
l_magic(_, Key, D) :-
        atom(Key),
        pp_write((':- ')), pp_write(Key),
        nl, tab(D).

l_magic_nl(2, C) :- !, pp_write(C),nl.
l_magic_nl(_, _).

l_magic((A;B), L, R, D) :- !,
        l_magic(A, L, 1, D), !,
        l_magic(B, 1, R, D).
l_magic(Conj,  L, R, D) :-
        E is D+8,
        M is L+3,
        l_clauses(Conj, M, 1, E),
        nl, tab(D),
        l_magic2(R, ')' ).

l_magic2(2, C) :- !, pp_write(C).
l_magic2(_, _).

pp_write(C):-write_unquoted(C).

top_writeq_atom(Pred):-writeq(Pred).
 

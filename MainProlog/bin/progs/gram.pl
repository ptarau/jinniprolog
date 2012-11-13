top_term(HB) --> top_term(eoc,HB).

top_term(End,HB) --> term(H), body(End,H,HB).

body(End,H,':-'(H,Bs)) --> [iff],conj_seq(Bs),[End].
body(End,H,H) --> [End].

term(V) --> [var(V)].
term(N) --> [num(N)].
term(T) --> [const(F)],args(Xs),{T=..[F|Xs]}.
term(L) --> [lbra],term_list(L).
term(S) --> [lpar],term(S).

args([T|Ts]) --> [lpar],term(T),arglist(Ts).
args([]) --> [].

arglist([]) --> [rpar].
arglist([T|Ts]) --> [comma],term(T),arglist(Ts).

term_list([]) --> [rbra],!.
term_list([T|Ts]) --> term(T),term_list_cont(Ts).

term_list_cont([T|Ts]) --> [comma], term(T),term_list_cont(Ts).
term_list_cont(T) --> [bar], term(T), [rbra].
term_list_cont([]) --> [rbra].

conj_seq((X,Xs))-->term(X),conj_list(Xs).

conj_list((T,Ts)) --> [comma],!,term(T),conj_list(Ts).
conj_list(true) --> [].

/* example

?- top_term(Term,[const(a),iff,const(b),comma,const(c),comma,const(d),eoc],[]).

Term = (a :- (b , c , d , true)) ;

*/
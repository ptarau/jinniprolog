/* assumption grammar based simple Prolog parser - uses assumptions.pl 
   requires small change in DCG preporocessor to map # to #: as #/3 is
   an arithmetic operation.
*/

% ?-list2clause("f(X):-g(X),h(X).",Term).

list2clause(Chars,Clause):-
  tokenizer(Chars,Tokens),
  parser(Tokens,Clause).

% Horn clause + disj + if-then-else parser

parser(Tokens,Term):-parser(Tokens,Term,_,_).

parser(Tokens,Term)-->
  '#<'(Tokens),
    top_term(eoc,Term),
  '#>'([]).

top_term(End,HB) --> term(H), body(End,H,HB).

match_end(End)--> #X, {X==End}.

test_end(End)--> '#>'(Xs),{nonvar(Xs),Xs=[End|_]}.

body(End,H,':-'(H,Bs)) --> #iff,conj_seq(Bs),match_end(End).
body(End,H,H) --> match_end(End).

term(V) --> #var(T),'#='(var(T,V)). /* uses "timeless assumptions" to associate unique vars to var names */
term(N) --> #num(N).
term(T) --> #const(F),args(Xs),{T=..[F|Xs]}.
term(L) --> #lbra,term_list(L).
term(S) --> #lpar,spec_term(S).

args([T|Ts]) --> #lpar,term(T),arglist(Ts).
args([])-->[].

arglist([]) --> #rpar.
arglist([T|Ts]) --> #comma,term(T),arglist(Ts).

term_list([])--> #rbra,!.
term_list([T|Ts]) --> term(T),term_list_cont(Ts).

term_list_cont([T|Ts])--> #comma, term(T),term_list_cont(Ts).
term_list_cont(T)--> #bar, term(T), #rbra.
term_list_cont([])--> #rbra.

conj_seq(Xs)-->seq(comma,',',term,Xs,_End).

seq(InCons,OutCons,Inner,Bs,End)--> 
  dcg_call_fun(Inner,B),
  cons_args(InCons,OutCons,Inner,B,Bs,End).

cons_args(InCons,OutCons,Inner,G,T,End) --> #InCons, !, 
  {T=..[OutCons,G,Gs]},
  dcg_call_fun(Inner,NewG),
  cons_args(InCons,OutCons,Inner,NewG,Gs,End).   
cons_args(_,_,_,G,G,End) --> test_end(End). 

spec_term(Xs)--> disj_seq(Xs),#rpar.
spec_term(Xs)--> top_term(rpar,Xs).

disj_seq(Xs)-->seq(disj,';',disj_term,Xs,_).

disj_term(T)--> seq(comma,',',term,Xs,End),disj_term_cont(End,Xs,T).

disj_term_cont(if,Xs,(Xs->Then))--> #if, seq(comma,',',term,Then,_).
disj_term_cont(disj,Xs,Xs)-->[].
disj_term_cont(rpar,Xs,Xs)-->[].

% tokenizer

tokenizer(Cs,Ws):-tokenizer(Cs,Ws,_,_).

tokenizer(Cs,Ws)--> '#<'([32|Cs]),words(Ws),!,'#>'([]).

words(Ws)-->dcg_star(word,Ws),space.

word(W)-->space,token(W).

token(lpar)-->c("(").
token(rpar)-->c(")").
token(lbra)-->c("[").
token(rbra)-->c("]").
token(bar)-->c("|").
token(comma)-->c(",").
token(disj)-->c(";").
token(if)-->c("->").
token(eoc)-->c(".").
token(iff)-->c(":-").
token(Token)-->token(F,Xs),{name(N,Xs)},{Token=..[F,N]}.

token(num,Xs) --> dcg_plus(is_digit,Xs).
token(const,Xs) --> only_one(is_punct,Xs).
token(F,Xs) --> #X,sym(X,F,Xs).

sym(X,var,[X|Xs])-->{is_maj(X)},!,dcg_star(is_letter,Xs).
sym(X,const,[X|Xs])-->{is_min(X)},dcg_star(is_letter,Xs).

c([])-->[].
c([X|Xs]) --> #X,c(Xs).

space-->dcg_star(is_space,_).

% regexp tools with  AGs + high order

only_one(F,[X])--> dcg_call_fun(F,X).

dcg_star(F,[X|Xs])--> dcg_call_fun(F,X),!,dcg_star(F,Xs).
dcg_star(_,[])-->[].

dcg_plus(F,[X|Xs])--> dcg_call_fun(F,X),dcg_star(F,Xs).

dcg_call_fun(F,X,S1,S2):-FX=..[F,X,S1,S2],topcall(FX). %,println(called=FX).

% recognizers - from BinProlog

is_space(X)--> #X, {member(X,[32,7,9,10,13])}.

is_letter(X)--> #X, {is_an(X)}.

is_punct(X)--> #X, {(is_spec(X);member(X,"!;`""'[]{}*"))}.

is_digit(X)--> #X, {is_num(X)}.

is_num(X):-member(X,"0123456789").

is_maj(X):-member(X,"_ABCDEFGHIJKLMNOPQRSTUVWXYZ").
is_min(X):-member(X, "abcdefghijklmnopqrstuvwxyz").
is_spec(X):-member(X,"#$&*+-./:<=>?@\^`").

is_an(X):-is_min(X).
is_an(X):-is_maj(X).
is_an(X):-is_num(X).

% tests

data(
"f(X,s(X))."
).
data(
"f:-g,h."
).
data(
"f(X,s(X)):-
   a(Y1,13,2,  Y1 ),!,
   g(X,b).").
data(
"a([X|Xs],Ys,[X|Zs]):-a(Xs,Ys,Zs)."
).
data(
"go(Xs,Ys):-a([1,2,3],[4,5|Xs],Ys)."
).

data(
"a(X):- (a,b(X),c), d(X)."
).

data(
"b(X):- ((a(X);b(X));c(X)), d(X),e(X)."
).

data(
"c(X):- ( a(X) -> b(X) ; c(X) )."
).

data(
"d(X):- 
   ( a(X), b(X) -> c,d,e ; c(X)->d ;  f(X),g,((h)) 
   ).
"
).

data("d((H:-B)):-a(H),d((B->t;f)),show(A,B,(A:-B)).").

test:-data(Cs),writeln(Cs),(tokenizer(Cs,Ws)->write(Ws),nl,write('!!!yes');write('no!!!')),nl,nl,fail.

writeln([]):-nl.
writeln([X|Xs]):-put(X),writeln(Xs).

go:-
   data(Cs),
   writeln(Cs),
   (list2clause(Cs,T)->
     write(T),nl,
     M='yes!!!'
     ;
     M='no!!!'
   ),
   write(M),nl,nl,
   fail.

ag_test:-phrase((
  '#<'([a,b,c]),
  '#+'(t(99)),
  '#='(s(X)),
  '#='(s(Y)),
  '#:'(X),
  '#>'(As)),Xs,Ys),
  println(X+Y+As+Xs+Ys).
  
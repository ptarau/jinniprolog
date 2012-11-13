% ?-list2clause("f(X):-g(X),h(X).",Term).

list2clause(Chars,Clause):-
  tokenizer(Chars,Tokens),
  parser(Tokens,Clause).

% Horn clause + disj + if-then-else parser

parser(Tokens,Term):-parser(Tokens,Term,_,_).

parser(Tokens,Term)-->
  '#<'(Tokens),
    top_term(eoc,Term,_Dict),
  '#>'([]).

top_term(End,HB,D) --> term(H,D), body(End,H,HB,D).

match_end(End)--> #X, {X==End}.

test_end(End)--> '#>'(Xs),{nonvar(Xs),Xs=[End|_]}.

body(End,H,':-'(H,Bs),D) --> #iff,conj_seq(Bs,D),match_end(End).
body(End,H,H,_) --> match_end(End).

check_var(_,_).

term(V,D) --> #var(T),{check_var(var(T,V),D)}.
term(N,_) --> #num(N).
term(T,D) --> #const(F),args(Xs,D),{T=..[F|Xs]}.
term(L,D) --> #lbra,term_list(L,D).
term(S,D) --> #lpar,spec_term(S,D).

args([T|Ts],D) --> #lpar,term(T,D),arglist(Ts,D).
args([],_)-->[].

arglist([],_) --> #rpar.
arglist([T|Ts],D) --> #comma,term(T,D),arglist(Ts,D).

term_list([],_)--> #rbra,!.
term_list([T|Ts],_) --> term(T,D),term_list_cont(Ts,D).

term_list_cont([T|Ts],D)--> #comma, term(T,D),term_list_cont(Ts,D).
term_list_cont(T,D)--> #bar, term(T,D), #rbra.
term_list_cont([],_)--> #rbra.

conj_seq(Xs,D)-->seq(comma,',',term,Xs,_End,D).

seq(InCons,OutCons,Inner,Bs,End,D)--> 
  dcg_call(Inner,B,D),
  cons_args(InCons,OutCons,Inner,B,Bs,End,D).

cons_args(InCons,OutCons,Inner,G,T,End,D) --> #InCons, !, 
  {T=..[OutCons,G,Gs]},
  dcg_call(Inner,NewG,D),
  cons_args(InCons,OutCons,Inner,NewG,Gs,End,D).   
cons_args(_,_,_,G,G,End,_) --> test_end(End). 

spec_term(Xs,D)--> disj_seq(Xs,D),#rpar.
spec_term(Xs,D)--> top_term(rpar,Xs,D).

disj_seq(Xs,D)-->seq(disj,';',disj_term,Xs,_,D).

disj_term(T,D)--> seq(comma,',',term,Xs,End,D),disj_term_cont(End,Xs,T,D).

disj_term_cont(if,Xs,(Xs->Then),D)--> #if, seq(comma,',',term,Then,_,D).
disj_term_cont(disj,Xs,Xs,_)-->[].
disj_term_cont(rpar,Xs,Xs,_)-->[].

% tokenizer

tokenizer(Cs,Ws):-tokenizer(Cs,Ws,_,_).

tokenizer(Cs,Ws)--> '#<'([32|Cs]),words(Ws),!,'#>'([]).

words(Ws)-->star(word,Ws),space.

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

token(num,Xs) --> plus(is_digit,Xs).
token(const,Xs) --> one(is_punct,Xs).
token(F,Xs) --> #X,sym(X,F,Xs).

sym(X,var,[X|Xs])-->{is_maj(X)},!,star(is_letter,Xs).
sym(X,const,[X|Xs])-->{is_min(X)},star(is_letter,Xs).

c([])-->[].
c([X|Xs]) --> #X,c(Xs).

space-->star(is_space,_).

% regexp tools with  AGs + high order

one(F,[X])--> dcg_call(F,X).

star(F,[X|Xs])--> dcg_call(F,X),!,star(F,Xs).
star(_,[])-->[].

plus(F,[X|Xs])--> dcg_call(F,X),star(F,Xs).

dcg_call(F,X,D,S1,S2):-FX=..[F,X,D,S1,S2],topcall(FX). %,println(called=FX).

dcg_call(F,X,S1,S2):-FX=..[F,X,S1,S2],topcall(FX). %,println(called=FX).

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
  
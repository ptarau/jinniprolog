% see xprolog/jsat for examples

ncompile(File):-ncompile(File,mem).

ncompile(File,Mode):-
  rollback,
  symcat('$end',File,EF),
  advance_code_top,
  namecat(File,'','.nat',NatFile),
  nat2clauses(NatFile,Clauses),
  foreach(member((H:-B),Clauses),cc(Mode,(H:-B))),
  jterminate(EF,Mode),
  foreach(
    member(':-'(B),Clauses),
    (pp_clause(query=B),topcall(B),pp_clause(B),fail)
  ).

nconsult(File):-
  db_clean,
  namecat(File,'','.nat',NatFile),
  nat2clauses(NatFile,Clauses),
  foreach(member((H:-B),Clauses),assert((H:-B))),
  foreach(
    member(':-'(B),Clauses),
    (pp_clause(query=B),topcall(B),pp_clause(B),fail)
  ).
    
% natural language to disjunctive datalog translator

nat2pro(File):-
  namecat(File,'','.nat',NatFile),
  namecat(File,'','.pro',ProFile),
  nat2clauses(NatFile,Clauses),
  tell(ProFile),
  map(pp_clause,Clauses),
  nl,
  told.

nat2clauses(NatFile,Clauses):-
  file2chars(NatFile,Codes),
  nat2log(Codes,Clauses).
    
nat2log(Codes,Clauses):-
  nparse_codes(Codes,Parsed),
  %println('PARSED'),map(pp_clause,Parsed),
  map(to_log,Parsed,Clauses).

to_log(def(Hs,Bs),(H:-B)):-to_datalog(Hs,H),to_datalog(Bs,B).
to_log(query(Bs),':-'(B)):-to_datalog(Bs,B).
to_log(gen(Hs,Bs,Gs),('@'((H:-B)):-G)):-to_datalog(Hs,H),to_datalog(Bs,B),to_datalog(Gs,G).

% parser to generic tree format

nparse_codes(Codes,Parsed):-
  codes_to_words(Codes,Ws),
  nparse_words(Ws,Parsed).

nparse_words(Ws,Parsed):-tparse(Parsed,Ws,[]).

tparse([])-->[],!.
tparse([D|Cs])-->rword(def),!,dparse(D),tparse(Cs).
tparse([D|Cs])-->rword(gen),!,gparse(D),tparse(Cs).
tparse([Q|Cs])-->rword(query),!,qparse(Q),tparse(Cs).

dparse(def(Head,Body))-->
  hparse(Head,Dict),
  !,
  bparse(Body,Dict).

gparse(gen(Head,Body,Grounder))-->
  iparse(Head,Dict),
  !,
  hparse(Body,Dict),
  !,
  bparse(Grounder,Dict).
  
/*
sparse([],End,_Dict)-->[End],!.
sparse([W|Ws],End,Dict)-->oneword(W,Dict),sparse(Ws,End,Dict).
*/

sparse(disj(Ds),End,Dict)-->disj_parse(Ds,Dict),rword(End),!.

disj_parse([conj(D)|Ds],Dict)-->conj_parse(D,Dict),rword(disj),!,disj_parse(Ds,Dict).
disj_parse([conj(D)],Dict)-->conj_parse(D,Dict).

conj_parse([D|Ds],Dict)-->lit_parse(D,Dict),rword(conj),!,conj_parse(Ds,Dict).
conj_parse([D],Dict)-->lit_parse(D,Dict).

lit_parse(not(term(T)),Dict)-->rword(neg),!,term_parse(T,Dict).
lit_parse(term(T),Dict)-->term_parse(T,Dict).

term_parse([T|Ts],Dict)-->oneword(T,Dict),term_parse(Ts,Dict),!.
term_parse([T],Dict)-->oneword(T,Dict).

qparse(query(Ws))-->sparse(Ws,eoq,_Dict).
hparse(Ws,Dict)-->sparse(Ws,neck,Dict).
bparse(Ws,Dict)-->sparse(Ws,eod,Dict).
iparse(Ws,Dict)-->sparse(Ws,if,Dict).

avoid_next(Xs)-->[X],{member(X,Xs)},!,{fail}. % ??
avoid_next(_)-->[].

rwords(eoq,['?']).
rwords(eod,['.']).
rwords(neck,[as,be,using]).
rwords(if,[if]).
rwords(def,[define,let,remember,assume]).
rwords(gen,[generate,ground]).
rwords(query,[which,what,who,asking]).
rwords(disj,[or]).
rwords(conj,[and]).
rwords(neg,[not]).

an_rword(W,KW):-rwords(KW,Ws),member(W,Ws).

an_rword(W):-an_rword(W,_).

rword(KW)-->[W],{rwords(KW,Ws),member(W,Ws)},!.

oneword(W,Dict)-->[X],{is_oneword(X,W,Dict)}.

is_oneword(W,_No,_Dict):-an_rword(W),!,fail. % vars fit here
is_oneword(W,v(W,X),Dict):-onevar(W),!,intree(Dict,W,X).
is_oneword(W,c(W),_Dict).

onevar(V):-atom_codes(V,[X|_]),to_upper_char(X,L),!,L=:=X.

% DCG debugger

tt-->[T],{println(here=T),fail}.
tt-->[].

% var allocator

intree(t(K,X,_,_),K,V):-!,V=X.
intree(t(K0,_X,Left,_Right),K,X):-compare(<,K,K0),!,intree(Left,K,X).
intree(t(_K,_X,_Left,Right),K,X):-intree(Right,K,X).

tree2keys(T,Es):-tree2keys(T,Es,[]).

tree2keys(T)-->{var(T)},!,[].
tree2keys(t(K,_X,L,R))-->[K],tree2keys(L),tree2keys(R).

tree2vals(T,Es):-tree2vals(T,Es,[]).

tree2vals(T)-->{var(T)},!,[].
tree2vals(t(_K,X,L,R))-->[X],tree2vals(L),tree2vals(R).

tree2assoc(T,Es):-tree2assoc(T,Es,[]).

tree2assoc(T)-->{var(T)},!,[].
tree2assoc(t(K,X,L,R))-->[v(K,X)],tree2assoc(L),tree2assoc(R).

% disjunctive datalog generator

to_datalog(disj(Cs),Rs):-to_ddisj(Cs,Rs).

to_ddisj([],fail).
to_ddisj([conj(C)|Cs],RRs):-to_dconj(C,R),more_to_disj(Cs,R,RRs).

more_to_disj([],R,R).
more_to_disj([C|Cs],R,(R;Rs)):-to_ddisj([C|Cs],Rs).

to_dconj([],true).
to_dconj([C|Cs],RRs):-to_dlit(C,R),more_to_conj(Cs,R,RRs).

more_to_conj([],R,R).
more_to_conj([C|Cs],R,(R,Rs)):-to_dconj([C|Cs],Rs).

to_dlit(not(T),not(R)):- !,to_dterm(T,R).
to_dlit(T,R):-to_dterm(T,R).

to_dterm(term([c(P)|Ws]),T):-map(to_darg,Ws,Ds),T=..[P|Ds].

to_darg(c(C),C).
to_darg(v(_Name,V),V).

% tools

codes_to_words(Cs,Ws):-
  clean_codes(Cs,NewCs),
  codes_words(NewCs,Ws).

clean_codes([],[]):-!.
clean_codes([W|Ws],Ns):-
  member(W,[10,13]),
  !,
  clean_codes(Ws,Ns).
clean_codes([W|Ws],[W|Ns]):-
  clean_codes(Ws,Ns).

% tests

ntest1:-
  nparse_words(
   [define,blue,sky,as,true,'.',
    define,red,apple,as,true,'.',
    define,colored,'X',as,red,'X',or,blue,'X','.',
    define,hot, 'X', as, red, 'X', and, not, blue, 'X','.',
    which,colored,'X','?'
  ],Cs),
  pp_clause(Cs).

ntest2:-
  nparse_codes(
    "define blue sky as true.
     define red apple as true.
     define colored X as red X or blue X.
     define hot X as red X and not blue X.
     which colored X?",
  Cs),
  pp_clause(Cs).
 
ntest3:-
  nat2log(
    "define colored as red or blue and boring.",
  Cs),
  map(pp_clause,Cs).

ntest4:-
  nat2log(
    "generate likes X Y if true using happy X and good Y.",
  Cs),
  map(pp_clause,Cs).

ntest5:-
  nparse_words(
   [generate,
       fail, if, colored,'X','C',and,colored,'Y', 'C',
       using,
       edge,'X','Y',and, color,'C','.'
  ],Cs),
  pp_clause(Cs).

ntest6:-
  nat2log(
    "generate fail if colored X C using true.",
  Cs),
  map(pp_clause,Cs).

ntest7:-
  nat2log(
    "generate 
       fail if colored X C and colored Y C
     using
       edge X Y and color C.",
    Cs),
  pp_clause(Cs).

ntest8:-
  nat2log(
    "define blue sky as true.
     define red apple as true.
     define colored X as red X or blue X.
     define hot X as red X and not blue X.
     which colored X?",
  Cs),
  map(pp_clause,Cs).
  
% end


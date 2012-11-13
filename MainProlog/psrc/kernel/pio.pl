full_parser(on):-set_global_prop(parser,full).
full_parser(off):-set_global_prop(parser,simple).

check_full_parser:-get_global_prop(parser,full),init_ops.
 
r_term_of(F,C):-
  p_term_of(F,C0),
  ( C0=':-'([_|_])->arg(1,C0,Fs),member(IF,Fs),r_term_of(IF,C)  
  ; C=C0
  ).

p_term_of(F,C):-
  find_file(F,File),
  % println('using full parser on file:'(File)),
  seeing(F0),
  see(File),
  repeat,
    p_read(C0),
    ( C0==end_of_file-> 
       !,seen,see(F0),fail
     ; C=C0
     ).
     	
quiet(1).
seeing_at(Byte):-Byte=0.
get_lineno(L):-get_line_number(L).
  
p_ttyprin(X):-ttyout(write(X)).
p_ttyprint(X):-ttyout((write(X),nl)).
p_ttycwrite(X):-ttyout(fast_write(X)).
p_ttycwriteln(X):-ttyout((fast_write(X),nl)).
p_ttynl:-ttyout(nl).
p_ttyput(X):-ttyout(put_code(X)).
display(X):-ttyout(portable_display(X)).

p_print(X):-portable_print(X).
p_write(X):-portable_write(X).
p_writeq(X):-portable_writeq(X).

p_read(X):-r_term(X,_).
p_top_read_term(T,V) :- p_read_term(T,V).
p_read_term(T,Vs) :- r_term(T,L),r_vars(L,Vs).

read_clause(EC):-
  read_with_singletons(C,Vs,Ss),
  std_expand_term(C,EC),
  warn_singletons(Ss,Vs,C).

read_with_singletons(X,Vs,Ss):-r_term(X,Vs),singletons(Ss,Vs).

warn_singletons([S|Ss],Vs,C):-
   get_verbosity(N),
   N>=2,
   % quiet(Q),Q=<2,
   warn_singletons1(C,[S|Ss],Vs),
   fail.
warn_singletons(_,_,_).

warn_singletons1(':-'(_),_,_):-!.
warn_singletons1(C,Ss,Vs):-
  add_true(C,(H:-_)),
  seeing_at(Byte),get_lineno(L),
  melt_varnames(Vs),
  ttyout((
    print(Ss),
    print('** warning singleton_variables=>'(line=L,byte=Byte)),nl,
    print('=> '),print(H),fast_write(':-...'),nl,
    nl
  )),
  !.

melt_varnames([]).
melt_varnames([var(N,V,_)|Xs]) :- V=N, melt_varnames(Xs).

read_or_fail(T,Vs) :- r_term1(T,L),r_vars(L,Vs).

r_vars([],[]).
r_vars([var(N,V,_O)|Vs],[N=V|Es]):-r_vars(Vs,Es).

%  awkward O,I order allowing member(Result,Options),call(Result,Vars)
singletons(ONs,IVs):-findall(N,member(var(N,_V,s(0)),IVs),ONs).

ttyin(ReadOp):-seeing(user),!,ReadOp.
ttyin(ReadOp):-
  seeing(F),
  see(user),
  ReadOp,
  %seing(user),
  see(F).

ttyout(_):-quiet(Q),Q>9,!. % quiet - suppresses tty output
ttyout(WriteOp):-telling(user),!,WriteOp.
ttyout(WriteOp):-
  telling(F),
  tell(user),WriteOp,flush,
  % telling(user),
  tell(F).


%   r_term(?Answer, ?Variables)
%   reads a term from the current input stream and unifies it with
%   Answer.  Variables is bound to a list of [Atom=Variable] pairs.

r_term(Answer, Variables) :-
	for(_,1,3), % at most 3 times
          r_term1(Term,Vars),
	!,
	Answer = Term,
        Variables = Vars.

r_term1(Term,Variables):-
       r_term2(Term,Variables,Ok),
       clean_up_syntax_errors,
       Ok=true.

r_term2(Term,Variables,Ok):-
	 read_tokens(Tokens, Variables),
	 r_and_check(Tokens,Term),
         !,
         Ok=true.
r_term2(_,_,fail).

clean_up_syntax_errors:-
  K1=syntax_error,K2=length,
  bb_val(K1,K2,_),bb_rm(K1,K2),
  !.
clean_up_syntax_errors.

r_and_check(Tokens,Term):-
	rt(Tokens, 1200, Term, LeftOver),
	all_read(LeftOver).
r_and_check(Tokens,_):-
	syntax_error(Tokens).

%   all_read(+Tokens)
%   checks that there are no unparsed tokens left over.

all_read([]) :- !.
all_read(S) :-
	syntax_error([operator,expected,after,expression], S).


%   expect(Token, TokensIn, TokensOut)
%   reads the next token, checking that it is the one expected, and
%   giving an error message if it is not.  It is used to look for
%   right brackets of various sorts, as they're all we can be sure of.

expect(Token, [Token|Rest], Rest) :- !.
expect(Token, S0, _) :-
	syntax_error([Token,or,operator,expected], S0).


%   I want to experiment with having the operator information held as
%   ordinary Prolog facts.  For the moment the following predicates
%   remain as interfaces to current_op. (i.e unfolded to val/3 - Paul Tarau)
%   prefixop(O -> Self, Rarg)
%   postfixop(O -> Larg, Self)
%   infixop(O -> Larg, Self, Rarg)


prefixop(Op, Prec, Prec) :-
	get_op0(Op, prefixop, fy, Prec), !.
prefixop(Op, Prec, Less) :-
	get_op0(Op, prefixop, fx, Prec),
	Less is Prec-1.

postfixop(Op, Prec, Prec) :-
	get_op0(Op, postfixop, yf, Prec), !.
postfixop(Op, Less, Prec) :-
	get_op0(Op, postfixop, xf, Prec), Less is Prec-1.

infixop(Op, Less, Prec, Less) :-
	get_op0(Op, infixop, xfx, Prec ), !, Less is Prec-1.
infixop(Op, Less, Prec, Prec) :-
	get_op0(Op, infixop, xfy, Prec), !, Less is Prec-1.
infixop(Op, Prec, Prec, Less) :-
	get_op0(Op, infixop, yfx, Prec), Less is Prec-1.

ambigop(F, L1, O1, R1, L2, O2) :-
	postfixop(F, L2, O2),
	infixop(F, L1, O1, R1).

%   rt(+TokenList, +Precedence, -Term, -LeftOver)
%   parses a Token List in a context of given Precedence,
%   returning a Term and the unread Left Over tokens.

rt([], _, _, _) :- syntax_error([expression,expected], []).
rt([Token|RestTokens], Precedence, Term, LeftOver) :-
	rts(Token, RestTokens, Precedence, Term, LeftOver).

%   rts(+Token, +RestTokens, +Precedence, -Term, -LeftOver)

rts(var(Variable,_), ['('|S1], Precedence, Answer, S) :- !,
	rt(S1, 999, Arg1, S2),
	r_args(S2, RestArgs, S3), !,
	exprtl0(S3, apply(Variable,[Arg1|RestArgs]), Precedence, Answer, S).
rts(var(Variable,_), S0, Precedence, Answer, S) :- !,
	exprtl0(S0, Variable, Precedence, Answer, S).
rts(atom(-), [integer(box(Integer,_))|S1], Precedence, Answer, S) :-
	Negative is 0-Integer, !,
	exprtl0(S1, Negative, Precedence, Answer, S).
rts(atom(-), [atom(F)|S1], Precedence, Answer, S) :-
	float(F),
	float_minus(F,Negative),!,
	exprtl0(S1, Negative, Precedence, Answer, S).
rts(atom(Functor), ['('|S1], Precedence, Answer, S) :- !,
	rt(S1, 999, Arg1, S2),
	r_args(S2, RestArgs, S3),
	Term =.. [Functor,Arg1|RestArgs], !,
	exprtl0(S3, Term, Precedence, Answer, S).
rts(atom(Functor), S0, Precedence, Answer, S) :-
	prefixop(Functor, Prec, Right), !,
	after_prefix_op(Functor, Prec, Right, S0, Precedence, Answer, S).
rts(atom(Atom), S0, Precedence, Answer, S) :- !,
	exprtl0(S0, Atom, Precedence, Answer, S).
rts(integer(box(Integer,_)), S0, Precedence, Answer, S) :- !,
	exprtl0(S0, Integer, Precedence, Answer, S).
rts('[', [']'|S1], Precedence, Answer, S) :- !,
	exprtl0(S1, [], Precedence, Answer, S).
rts('[', S1, Precedence, Answer, S) :- !,
	rt(S1, 999, Arg1, S2),
	r_list(S2, RestArgs, S3), !,
	exprtl0(S3, [Arg1|RestArgs], Precedence, Answer, S).
rts('(', S1, Precedence, Answer, S) :- !,
	rt(S1, 1200, Term, S2),
	expect(')', S2, S3), !,
	exprtl0(S3, Term, Precedence, Answer, S).
rts('((', S1, Precedence, Answer, S) :- !,
	rt(S1, 1200, Term, S2),
	expect(')', S2, S3), !,
	exprtl0(S3, Term, Precedence, Answer, S).
rts('{', ['}'|S1], Precedence, Answer, S) :- !,
	exprtl0(S1, '{}', Precedence, Answer, S).
rts('{', S1, Precedence, Answer, S) :- !,
	rt(S1, 1200, Term, S2),
	expect('}', S2, S3), !,
	exprtl0(S3, '{}'(Term), Precedence, Answer, S).
rts(string(List), S0, Precedence, Answer, S) :- !,
	exprtl0(S0, List, Precedence, Answer, S).
rts(Token, S0, _, _, _) :-
	syntax_error([Token,cannot,start,an,expression], S0).


%   r_args(+Tokens, -TermList, -LeftOver)
%   parses {',' expr(999)} ')' and returns a list of terms.

r_args([','|S1], [Term|Rest], S) :- !,
	rt(S1, 999, Term, S2), !,
	r_args(S2, Rest, S).
r_args([')'|S], [], S) :- !.
r_args(S, _, _) :-
	syntax_error([',)',expected,in,arguments], S).


%   r_list(+Tokens, -TermList, -LeftOver)
%   parses {',' expr(999)} ['|' expr(999)] ']' and returns a list 
%   of terms.

r_list([','|S1], [Term|Rest], S) :- !,
	rt(S1, 999, Term, S2), !,
	r_list(S2, Rest, S).
r_list(['|'|S1], Rest, S) :- !,
	rt(S1, 999, Rest, S2), !,
	expect(']', S2, S).
r_list([']'|S], [], S) :- !.
r_list(S, _, _) :-
	syntax_error(['|]',expected,in,list], S).


%   after_prefix_op(+Op, +Prec, +ArgPrec, +Rest, +Precedence, 
%       -Ans, -LeftOver)

after_prefix_op(Op, Oprec, _, S0, Precedence, _, _) :-
	Precedence < Oprec, !,
        syntax_error([prefix,operator,Op,in,context,
                      with,precedence,Precedence],
	S0).
after_prefix_op(Op, Oprec, _, S0, Precedence, Answer, S) :-
	peepop(S0, S1),
	prefix_is_atom(S1, Oprec), % can't cut but would like to
	exprtl(S1, Oprec, Op, Precedence, Answer, S).
after_prefix_op(Op, Oprec, Aprec, S1, Precedence, Answer, S) :-
	rt(S1, Aprec, Arg, S2),
	Term =.. [Op,Arg], !,
	exprtl(S2, Oprec, Term, Precedence, Answer, S).


%   The next clause fixes a bug concerning "mop dop(1,2)" where
%   mop is monadic and dop dyadic with higher Prolog priority.

peepop([atom(F),'('|S1], [atom(F),'('|S1]) :- !.
peepop([atom(F)|S1], [infixop(F,L,P,R)|S1]) :- infixop(F, L, P, R).
peepop([atom(F)|S1], [postfixop(F,L,P)|S1]) :- postfixop(F, L, P).
peepop(S0, S0).


%   prefix_is_atom(+TokenList, +Precedence)
%   is true when the right context TokenList of a prefix operator
%   of result precedence Precedence forces it to be treated as an
%   atom, e.g. (- = X), p(-), [+], and so on.

prefix_is_atom([Token|_], Precedence) :- prefix_is_atom(Token, Precedence).

prefix_is_atom(infixop(_,L,_,_), P) :- L >= P.
prefix_is_atom(postfixop(_,L,_), P) :- L >= P.
prefix_is_atom(')', _).
prefix_is_atom(']', _).
prefix_is_atom('}', _).
prefix_is_atom('|', P) :- 1100 >= P.
prefix_is_atom(',', P) :- 1000 >= P.
prefix_is_atom([],  _).


%   exprtl0(+Tokens, +Term, +Prec, -Answer, -LeftOver)
%   is called by read/4 after it has read a primary (the Term).
%   It checks for following postfix or infix operators.

exprtl0([atom(F)|S1], Term, Precedence, Answer, S) :-
	ambigop(F, L1, O1, R1, L2, O2), !,
	(   exprtl([infixop(F,L1,O1,R1)|S1], 0, Term, Precedence, 
		Answer, S)
	;   exprtl([postfixop(F,L2,O2) |S1], 0, Term, Precedence, 
		Answer, S)
	).
exprtl0([atom(F)|S1], Term, Precedence, Answer, S) :-
	infixop(F, L1, O1, R1), !,
	exprtl([infixop(F,L1,O1,R1)|S1], 0, Term, Precedence, Answer, S).
exprtl0([atom(F)|S1], Term, Precedence, Answer, S) :-
	postfixop(F, L2, O2), !,
	exprtl([postfixop(F,L2,O2) |S1], 0, Term, Precedence, Answer, S).
exprtl0([','|S1], Term, Precedence, Answer, S) :-
	Precedence >= 1000, !,
	rt(S1, 1000, Next, S2), !,
	exprtl(S2, 1000, (Term,Next), Precedence, Answer, S).
exprtl0(['|'|S1], Term, Precedence, Answer, S) :-
	Precedence >= 1100, !,
	rt(S1, 1100, Next, S2), !,
	exprtl(S2, 1100, (Term;Next), Precedence, Answer, S).
exprtl0([Thing|S1], _, _, _, _) :-
	cant_follow_expr(Thing, Culprit), !,
	syntax_error([Culprit,follows,expression], [Thing|S1]).
exprtl0(S, Term, _, Term, S).

cant_follow_expr(atom(_),	atom).
cant_follow_expr(var(_,_),	variable).
cant_follow_expr(integer(_),	integer).
cant_follow_expr(string(_),	string).
cant_follow_expr('((',		bracket).
cant_follow_expr('(',		bracket).
cant_follow_expr('[',		bracket).
cant_follow_expr('{',		bracket).

exprtl([infixop(F,L,O,R)|S1], C, Term, Precedence, Answer, S) :-
	Precedence >= O, C =< L, !,
	rt(S1, R, Other, S2),
	Expr =.. [F,Term,Other], % !,
	exprtl(S2, O, Expr, Precedence, Answer, S).
exprtl([postfixop(F,L,O)|S1], C, Term, Precedence, Answer, S) :-
	Precedence >= O, C =< L, !,
	Expr =.. [F,Term],
	peepop(S1, S2),
	exprtl(S2, O, Expr, Precedence, Answer, S).
exprtl([','|S1], C, Term, Precedence, Answer, S) :-
	Precedence >= 1000, C < 1000, !,
	rt(S1, 1000, Next, S2), !,
	exprtl(S2, 1000, (Term,Next), Precedence, Answer, S).
exprtl(['|'|S1], C, Term, Precedence, Answer, S) :-
	Precedence >= 1100, C < 1100, !,
	rt(S1, 1100, Next, S2), !,
	exprtl(S2, 1100, (Term;Next), Precedence, Answer, S).
exprtl(S, _, Term, _, Term, S).


%   This business of syntax errors is tricky.  When an error is 
%   detected, we have to write out a message.  We also have to note 
%   how far it was to the end of the input, and for this we are 
%   obliged to use the data-base.  Then we fail all the way back to 
%   read(), and that prints the input list with a marker where the 
%   error was noticed.  If subgoal_of were available in compiled code 
%   we could use that to find the input list without hacking the 
%   data base.  The really hairy thing is that the original code 
%   noted a possible error and backtracked on, so that what looked 
%   at first sight like an error sometimes turned out to be a wrong 
%   decision by the parser.  This version of the parser makes
%   fewer wrong decisions, and my goal was to get it to do no
%   backtracking at all.  This goal has not yet been met, and it 
%   will still occasionally report an error message and then decide 
%   that it is happy with the input after all.  Sorry about that.


syntax_error(Message, List):-seeing(user),!,
  start_syntax_error(0,0,Message,List),
  fail.
syntax_error(Message, List) :-
	seeing(F),seeing_at(Byte),get_lineno(Line),
  see(user),
	  start_syntax_error(Line,Byte,Message,List),
	see(F),
	fail.

start_syntax_error(Line,Byte,Message,List):-
	length(List, Length),
	bb_def(syntax_error, length, err(Message,Line,Byte,Length)),!.
start_syntax_error(_,_,_,_).

syntax_error(List):-seeing(user),!,finish_syntax_error(List),fail.
syntax_error(List) :-
	seeing(F),see(user),
	finish_syntax_error(List),
	see(F),
	fail.

finish_syntax_error(List):-
	bb_val(syntax_error,length,err(Message,Line,Byte,AfterError)),
        bb_rm(syntax_error,length),
	ttynl,
          display('** SYNTAX ERROR ** LINE='),display(Line),
          display(' (approx.), BYTE='),display(Byte),display(':'),
	display_list(Message),
	length(List, Length),
	BeforeError is Length-AfterError,
	display_list(List, BeforeError),!.

display_list([Head|Tail]) :-
	ttyput(32),
	display_token(Head), !,
	display_list(Tail).
display_list([]) :-
	ttynl.

display_list(X, 0) :-
	display(' <HERE=> '), !,
	display_list(X, 99999).
display_list([Head|Tail], BeforeError) :-
	display_token(Head),
	ttyput(32),
	Left is BeforeError-1, !,
	display_list(Tail, Left).
display_list([], _) :-
	ttynl.

display_token(atom(X))	  :- float(X),!,write_float(X).
display_token(atom(X))	  :- !,display(X).
display_token(var(_,X))	  :- !,	display(X).
display_token(integer(box(I,_))) :- !,display(I).
display_token(string(Xs))  :- !,[Q]="""",
	det_append([Q|Xs],[Q],L),
	name(N,L),display(N).
display_token(X):-display(X).


% --------- rdtok.pl ----------------
%   File   : RDTOK.PL
%   Author : R.A.O'Keefe
%   Updated: 2 July 1984
%   Purpose: Tokeniser in reasonably standard Prolog.

/*  This tokeniser is meant to complement the library READ routine.
    It recognises Dec-10 Prolog with the following exceptions:

	%( is not accepted as an alternative to {

	%) is not accepted as an alternative to )

	NOLC convention is not supported (r_name could be made to 
		do it)

	,.. is not accepted as an alternative to | (hooray!)

	large integers are not read in as xwd(Top18Bits,Bottom18Bits)

	After a comma, "(" is read as '((' rather than '('.  This does 
		the parser no harm at all, and the Dec-10 tokeniser's 
		behaviour here doesn't actually buy you anything.  
		This tokeniser guarantees never to return '(' except 
		immediately after an atom, yielding '((' every
		other where.

    In particular, radix notation is EXACTLY as in Dec-10 Prolog 
    version 3.53.  Some times might be of interest.  Applied to an 
    earlier version of this file:

	this code took		    1.66 seconds
	the Dec-10 tokeniser took   1.28 seconds [DEC-10 assembler -Tim]
	A Pascal version took	    0.96 seconds

    The Dec-10 tokeniser was called via the old RDTOK interface, with
    which this file is compatible.  One reason for the difference in
    speed is the way variables are looked up: this code uses a linear
    list, while the Dec-10 tokeniser uses some sort of tree.  The 
    Pascal version is the program WLIST which lists "words" and their 
    frequencies.  It uses a hash table.  Another difference is the way 
    characters are classified: the Dec-10 tokeniser and WLIST have a 
    table which maps ASCII codes to character classes, and don't do 
    all this comparison and memberchking.  We could do that without 
    leaving standard Prolog, but what do you want from one evening's 
    work?
*/


%   read_tokens(TokenList, Dictionary)
%   returns a list of tokens.  It is needed to "prime" r_toks/2
%   with the initial blank, and to check for end of file.  The
%   Dictionary is a list of AtomName=Variable pairs in no particular 
%   order.  The way end of file is handled is that everything else 
%   FAILS when it hits character "-1", sometimes printing a warning.  
%   It might have been an idea to return the atom 'end_of_file' 
%   instead of the same token list that you'd have got from reading 
%   "end_of_file. ", but (1) this file is for compatibility, and (b) 
%   there are good practical reasons for wanting this behaviour.

read_tokens(TokenList, Dictionary) :-
  r_toks(32, Dict, ListOfTokens),
  append(Dict, [], Dict), !, %  fill in the "hole" at the end
  Dictionary = Dict,	     %  unify explicitly so we'll read and
  TokenList = ListOfTokens.  %  then check even with filled in arguments
read_tokens([atom(end_of_file)], []). %  Eof is all that can go wrong

r_toks(-1, _, _) :- !,	     %  -1 is the end-of-file character
	fail.			     %  in every standard Prolog
r_toks(Ch, Dict, Tokens) :-
	Ch =< 32,	     	     %  ignore layout.  CR, LF, and the
	!,			     %  Dec-10 newline character (31)
	get_code(NextCh),		     %  are all skipped here.
	r_toks(NextCh, Dict, Tokens).
r_toks(37, Dict, Tokens) :- !,	%  %comment
	repeat,				%  skip characters to a line
	    get_code(Ch),
	    is_terminator(Ch),
	!,	%  stop when we find one
	Ch =\= -1,			%  fail on EOF
	get_code(NextCh),
	r_toks(NextCh, Dict, Tokens).
r_toks(47, Dict, Tokens) :- !,	%  /*comment?
	get_code(NextCh),
	r_solidus(NextCh, Dict, Tokens).
r_toks(33, Dict, [atom(!)|Tokens]) :- !,	%  This is a special case so
	get_code(NextCh),			%  that !. reads as two tokens.
	r_after_atom(NextCh, Dict, Tokens).	%  It could be cleverer.
r_toks(40, Dict, ['(('|Tokens]) :- !,	%  NB!!!
	get_code(NextCh),
	r_toks(NextCh, Dict, Tokens).
r_toks(41, Dict, [')'|Tokens]) :- !,
	get_code(NextCh),
	r_toks(NextCh, Dict, Tokens).
r_toks(44, Dict, [','|Tokens]) :- !,
	get_code(NextCh),
	r_toks(NextCh, Dict, Tokens).
r_toks(59, Dict, [atom((;))|Tokens]) :- !,	%   ; is nearly a punctuation
	get_code(NextCh),			%   mark but not quite (e.g.
	r_toks(NextCh, Dict, Tokens).	%   you can :-op declare it).
r_toks(91, Dict, ['['|Tokens]) :- !,
	get_code(NextCh),
	r_toks(NextCh, Dict, Tokens).
r_toks(93, Dict, [']'|Tokens]) :- !,
	get_code(NextCh),
	r_toks(NextCh, Dict, Tokens).
r_toks(123, Dict, ['{'|Tokens]) :- !,
	get_code(NextCh),
	r_toks(NextCh, Dict, Tokens).
r_toks(124, Dict, ['|'|Tokens]) :- !,
	get_code(NextCh),
	r_toks(NextCh, Dict, Tokens).
r_toks(125, Dict, ['}'|Tokens]) :- !,
	get_code(NextCh),
	r_toks(NextCh, Dict, Tokens).
r_toks(46, Dict, Tokens) :- !,		%  full stop
	get_code(NextCh),				%  or possibly .=. &c
	r_fullstop(NextCh, Dict, Tokens).
r_toks(34, Dict, [string(S)|Tokens]) :- !,	%  "string"
	r_string(S, 34, NextCh),
	r_toks(NextCh, Dict, Tokens).
r_toks(39, Dict, [atom(A)|Tokens]) :- !,	%  'atom'
	r_string(S, 39, NextCh),
	name(A, S),		%  BUG: '0' = 0 unlike Dec-10 Prolog
	r_after_atom(NextCh, Dict, Tokens).
r_toks(Ch, Dict, [var(Var,Name)|Tokens]) :- is_maj(Ch),!,
	%  have to watch out for "_"
	r_name(Ch, S, NextCh),
	(  S = "_", Name = '_'		%  anonymous variable Var = _
	;  S=[C|_],
	   name(Name, S),		%  construct name
	   r_lookup(Dict, C, Name, Var)	%  lookup/enter in dictionary
	), !,
	r_toks(NextCh, Dict, Tokens).
r_toks(Ch, Dict, Tokens) :- is_num(Ch),!,
	r_integer(Ch, I, NextCh,Digits),
	r_toks(NextCh, Dict, Tokens1),
	try_float(Digits,Tokens1,I,Tokens).
r_toks(Ch, Dict, [atom(A)|Tokens]) :- is_min(Ch),!,
	r_name(Ch, S, NextCh),
	name(A, S),
	r_after_atom(NextCh, Dict, Tokens).
r_toks(Ch, Dict, [atom(A)|Tokens]) :-	% THIS MUST BE THE LAST CLAUSE
	get_code(AnotherCh),
	r_symbol(AnotherCh, Chars, NextCh),	% might read 0 chars
	name(A, [Ch|Chars]),			% so might be [Ch]
	r_after_atom(NextCh, Dict, Tokens).


% An easy patch to read.pl for basic float input - Paul Tarau, Oct. 1993

try_float(_Ds,[atom('.'),integer(box(_Fraq,FDs))|Ts],I,[atom(Float)|Ts1]):-!,
	try_float_exp(Ts,Ts1,Exp),name(NFraq,FDs),
	input_float(I,NFraq,Exp,Float).
try_float(Ds,Tokens,I,[integer(box(I,Ds))|Tokens]).

try_float_exp([atom(e)|Ts],Ts1,Exp):-try_float_exp1(Ts,Ts1,Exp),!.
try_float_exp([atom(EN)|Ts],Ts,Exp):- [E]="e",
	name(EN,[E|NL]),name(Exp,NL),integer(Exp),!.
try_float_exp(Ts,Ts,0).

try_float_exp1([atom('-'),integer(box(Exp,_))|Ts],Ts,Exp1):-!,
	Exp1 is 0-Exp.
try_float_exp1([atom('+'),integer(box(Exp,_))|Ts],Ts,Exp).

float_minus(F,NegF):-NegF is (1/2-F)-1/2.

input_float(I,F,E,R):-
  % println(input_float(I,F,E)),
  to_number(F,X),pow(10,E,P),R is I+(X*P).

%   The only difference between r_after_atom(Ch, Dict, Tokens) and
%   r_toks/3 is what they do when Ch is "(".  r_after_atom
%   finds the token to be '(', while r_toks finds the token to be
%   '(('.  This is how the parser can tell whether <atom> <paren> must
%   be an operator application or an ordinary function symbol 
%   application.  See the library file READ.PL for details.

r_after_atom(40, Dict, ['('|Tokens]) :- !,
	get_code(NextCh),
	r_toks(NextCh, Dict, Tokens).
r_after_atom(Ch, Dict, Tokens) :-
	r_toks(Ch, Dict, Tokens).


%   r_string(Chars, Quote, NextCh)
%   reads the body of a string delimited by Quote characters.
%   The result is a list of ASCII codes.  There are two complications.
%   If we hit the end of the file inside the string this predicate 
%   FAILS.  It does not return any special structure.  That is the 
%   only reason it can ever fail.  The other complication is that when
%   we find a Quote we have to look ahead one character in case it is 
%   doubled.  Note that if we find an end-of-file after the quote we 
%   *don't* fail, we return a normal string and the end of file 
%   character is returned as NextCh.  If we were going to accept 
%   C-like escape characters, as I think we should, this would need 
%   changing (as would the code for 0'x).  But the purpose of this 
%   module is not to present my ideal syntax but to present something 
%   which will read present-day Prolog programs.

r_string(Chars, Quote, NextCh) :-
	get_code(Ch),
	r_string(Ch, Chars, Quote, NextCh).

r_string(-1, _, Quote, -1) :-
	display('! end of file in: '), ttyput(Quote),
	display(token), ttyput(Quote), ttynl,
	!, fail.
r_string(Quote, Chars, Quote, NextCh) :- !,
	get_code(Ch),			% closing or doubled quote
	more_string(Ch, Quote, Chars, NextCh).
r_string(Char, [Char|Chars], Quote, NextCh) :-
	r_string(Chars, Quote, NextCh).	% ordinary character


more_string(Quote, Quote, [Quote|Chars], NextCh) :- !,
	r_string(Chars, Quote, NextCh).	% doubled quote
more_string(NextCh, _, [], NextCh).		% end



% r_solidus(Ch, Dict, Tokens)
%   checks to see whether /Ch is a /* comment or a symbol.  If the
%   former, it skips the comment.  If the latter it just calls 
%   r_symbol.  We have to take great care with /* comments to 
%   handle end of file inside a comment, which is why r_solidus/2 
%   passes back an end of file character or a (forged) blank that we 
%   can give to r_toks.


patch_slash(['(('|Xs],['('|Xs]):-!. % fixes bug with /\(a) -PT
patch_slash(Xs,Xs).

r_solidus(42, Dict, Tokens) :- !, % 42= '*'
	get_code(Ch),
	r_solidus(Ch, NextCh),
	r_toks(NextCh, Dict, Tokens).
r_solidus(Ch, Dict, [atom(A)|Tokens1]) :-
	r_symbol(Ch, Chars, NextCh),		% might read 0 chars
	name(A, [47|Chars]),
	r_toks(NextCh, Dict, Tokens),
        patch_slash(Tokens,Tokens1).

r_solidus(-1, -1) :- !,
	display('! end_of_file in /*.. comment'), ttynl.
r_solidus(42, LastCh) :-
	get_code(NextCh),
	NextCh =\= 47, !,	%  might be ^Z or * though
	r_solidus(NextCh, LastCh).
r_solidus(42, 32) :- !.	%  the / was skipped in the previous clause
r_solidus(_, LastCh) :-
	get_code(NextCh),
	r_solidus(NextCh, LastCh).


%   r_name(Char, String, LastCh)
%   reads a sequence of letters, digits, and underscores, and returns
%   them as String.  The first character which cannot join this sequence
%   is returned as LastCh.

r_name(Char, [Char|Chars], LastCh) :-
	is_an(Char),!,
	get_code(NextCh),
	r_name(NextCh, Chars, LastCh).
r_name(LastCh, [], LastCh).

%   r_symbol(Ch, String, NextCh)
%   reads the other kind of atom which needs no quoting: one which is
%   a string of "symbol" characters.  Note that it may accept 0
%   characters, this happens when called from r_fullstop.

r_symbol(Char, [Char|Chars], LastCh) :-
	is_spec(Char),
	get_code(NextCh),
	r_symbol(NextCh, Chars, LastCh).
r_symbol(LastCh, [], LastCh).


%   r_fullstop(Char, Dict, Tokens)
%   looks at the next character after a full stop.  There are
%   three cases:
%	(a) the next character is an end of file.  We treat this
%	    as an unexpected end of file.  The reason for this is
%	    that we HAVE to handle end of file characters in this
%	    module or they are gone forever; if we failed to check
%	    for end of file here and just accepted .<EOF> like .<NL>
%	    the caller would have no way of detecting an end of file.
%	(b) the next character is a layout character.  This is a
%	    clause terminator.
%	(c) the next character is anything else.  This is just an
%	    ordinary symbol and we call r_symbol to process it.

r_fullstop(-1, _, _) :- !,
	display('! end_of_file just after full_stop'), ttynl,
	fail.
r_fullstop(Ch, _, []) :-
	Ch =< 32, !.		% END OF CLAUSE
r_fullstop(Ch, Dict, [atom(A)|Tokens]) :-
	r_symbol(Ch, S, NextCh),
	name(A, [46|S]),
	r_toks(NextCh, Dict, Tokens).


%   r_integer is complicated by having to understand radix notation.
%   There are three forms of integer:
%	0 ' <any character>	- the ASCII code for that character
%	<digit> ' <digits>	- the digits, read in that base
%	<digits>		- the digits, read in base 10.
%   Note that radix 16 is not understood, because 16 is two digits,
%   and that all the decimal digits are accepted in each base (this
%   is also true of C).  So 2'89 = 25.  I can't say I care for this,
%   but it does no great harm, and that's what Dec-10 Prolog does.
%   The X =\= -1 tests are to make sure we don't miss an end of file
%   character.  The tokeniser really should be in C, not least to
%   make handling end of file characters bearable.  If we hit an end
%   of file inside an integer, r_integer will fail.

r_integer(BaseChar, IntVal, NextCh,Digits) :-
	Base is BaseChar - 48,
	get_code(Ch), [Dot]=".",
	r_int(Ch, Base, IntVal, NextCh, Ds), Digits=[Dot,BaseChar|Ds].
/*
 % Paul Tarau - float like 0.0003 used to become 0.3
	( name(Name,Digits),
	  errmes(number(Name),Digits)->true
        ; true
        ).
*/

r_int(-1,_, _, _, _):-!,fail.
r_int(39, 0, IntVal, NextCh, [39,IntVal]):-!,
	get_code(IntVal), IntVal =\= -1, get_code(NextCh).
r_int(39, Base, IntVal, NextCh, Ds):-
	r_digits(0, Base, IntVal, NextCh, Ds),!.
r_int(Ch,Base,IntVal, NextCh, Ds):-
	r_digs(Ch, Base, 10, IntVal, NextCh ,Ds).

r_digits(SoFar, Base, Value, NextCh, Ds) :-
	get_code(Ch),
	Ch =\= -1,
	r_digs(Ch, SoFar, Base, Value, NextCh, Ds).

r_digs(Digit, SoFar, Base, Value, NextCh, [Digit|Ds]) :-
	is_num(Digit),!,
	Temp is SoFar*Base, Temp1 is Temp-48, Next is Temp1+Digit,
	r_digits(Next, Base, Value, NextCh, Ds).
r_digs(LastCh, Value, _, Value, LastCh, []).

%   r_lookup is identical to memberchk except for argument order and
%   mode declaration.

r_lookup([var(Name,Var,Occ)|_], C, Name, Var) :- !,r_varcount(Occ,C).
r_lookup([_|T], C, Name, Var) :- r_lookup(T, C, Name, Var). 

r_varcount(Occ,C):-[C]="_",!,Occ=s(1).
r_varcount(Occ,_):-var(Occ),!,Occ=s(_).
r_varcount(s(1),_).

% - various char types now generated in builtins.pl - see headers.pl

is_maj(65).
is_maj(66).
is_maj(67).
is_maj(68).
is_maj(69).
is_maj(70).
is_maj(71).
is_maj(72).
is_maj(73).
is_maj(74).
is_maj(75).
is_maj(76).
is_maj(77).
is_maj(78).
is_maj(79).
is_maj(80).
is_maj(81).
is_maj(82).
is_maj(83).
is_maj(84).
is_maj(85).
is_maj(86).
is_maj(87).
is_maj(88).
is_maj(89).
is_maj(90).
is_maj(95).
is_maj(192).
is_maj(193).
is_maj(194).
is_maj(195).
is_maj(196).
is_maj(197).
is_maj(198).
is_maj(199).
is_maj(200).
is_maj(201).
is_maj(202).
is_maj(203).
is_maj(204).
is_maj(205).
is_maj(206).
is_maj(207).
is_maj(208).
is_maj(209).
is_maj(210).
is_maj(211).
is_maj(212).
is_maj(213).
is_maj(214).
is_maj(216).
is_maj(217).
is_maj(218).
is_maj(219).
is_maj(220).
is_maj(221).
is_maj(222).

is_min(97).
is_min(98).
is_min(99).
is_min(100).
is_min(101).
is_min(102).
is_min(103).
is_min(104).
is_min(105).
is_min(106).
is_min(107).
is_min(108).
is_min(109).
is_min(110).
is_min(111).
is_min(112).
is_min(113).
is_min(114).
is_min(115).
is_min(116).
is_min(117).
is_min(118).
is_min(119).
is_min(120).
is_min(121).
is_min(122).
is_min(223).
is_min(224).
is_min(225).
is_min(226).
is_min(227).
is_min(228).
is_min(229).
is_min(230).
is_min(231).
is_min(232).
is_min(233).
is_min(234).
is_min(235).
is_min(236).
is_min(237).
is_min(238).
is_min(239).
is_min(240).
is_min(241).
is_min(242).
is_min(243).
is_min(244).
is_min(245).
is_min(246).
is_min(248).
is_min(249).
is_min(250).
is_min(251).
is_min(252).
is_min(253).
is_min(254).

is_num(48).
is_num(49).
is_num(50).
is_num(51).
is_num(52).
is_num(53).
is_num(54).
is_num(55).
is_num(56).
is_num(57).

is_an(48).
is_an(49).
is_an(50).
is_an(51).
is_an(52).
is_an(53).
is_an(54).
is_an(55).
is_an(56).
is_an(57).
is_an(65).
is_an(66).
is_an(67).
is_an(68).
is_an(69).
is_an(70).
is_an(71).
is_an(72).
is_an(73).
is_an(74).
is_an(75).
is_an(76).
is_an(77).
is_an(78).
is_an(79).
is_an(80).
is_an(81).
is_an(82).
is_an(83).
is_an(84).
is_an(85).
is_an(86).
is_an(87).
is_an(88).
is_an(89).
is_an(90).
is_an(95).
is_an(97).
is_an(98).
is_an(99).
is_an(100).
is_an(101).
is_an(102).
is_an(103).
is_an(104).
is_an(105).
is_an(106).
is_an(107).
is_an(108).
is_an(109).
is_an(110).
is_an(111).
is_an(112).
is_an(113).
is_an(114).
is_an(115).
is_an(116).
is_an(117).
is_an(118).
is_an(119).
is_an(120).
is_an(121).
is_an(122).
is_an(192).
is_an(193).
is_an(194).
is_an(195).
is_an(196).
is_an(197).
is_an(198).
is_an(199).
is_an(200).
is_an(201).
is_an(202).
is_an(203).
is_an(204).
is_an(205).
is_an(206).
is_an(207).
is_an(208).
is_an(209).
is_an(210).
is_an(211).
is_an(212).
is_an(213).
is_an(214).
is_an(216).
is_an(217).
is_an(218).
is_an(219).
is_an(220).
is_an(221).
is_an(222).
is_an(223).
is_an(224).
is_an(225).
is_an(226).
is_an(227).
is_an(228).
is_an(229).
is_an(230).
is_an(231).
is_an(232).
is_an(233).
is_an(234).
is_an(235).
is_an(236).
is_an(237).
is_an(238).
is_an(239).
is_an(240).
is_an(241).
is_an(242).
is_an(243).
is_an(244).
is_an(245).
is_an(246).
is_an(248).
is_an(249).
is_an(250).
is_an(251).
is_an(252).
is_an(253).
is_an(254).

is_spec(35).
is_spec(36).
is_spec(38).
is_spec(42).
is_spec(43).
is_spec(45).
is_spec(46).
is_spec(47).
is_spec(58).
is_spec(60).
is_spec(61).
is_spec(62).
is_spec(63).
is_spec(64).
is_spec(92).
is_spec(94).
is_spec(96).
is_spec(126).

is_terminator(-1).
is_terminator(10).
is_terminator(13).

%   File   : WRITE.PL
%   Author : Richard A. O'Keefe.
%   Updated: 22 October 1984
%   Purpose: Portable definition of write/1 and friends.

/* minor changes: Paul Tarau 1992
- uses fast_write (written in C) for simple things
- disconects unimplemented current_... predicates
- minor change by replacing (strange) l_magic with l_magic_nl
- removed code not used in BinProlog
*/


% this can be safely overriden, it seems free of side effects on failure

% generic_write(Term):-assumed(write_style(M)),!,generic_write(Term,M).
generic_write(Term):-generic_write(Term,writeq).

generic_write(Term,Mode):- w_out(Term, Mode, 1200, punct,_).

portable_display(Term) :-
        w_out(Term, display).

portable_print(Term) :-
        w_out(Term, print).

portable_write(Term) :-
        w_out(Term, write).

portable_writeq(Term) :-
        w_out(Term, writeq).


w_out(Term,Style):-
  w_out(Term, Style, 1200, punct,_),
  fail.
w_out(_,_).
        
p_top_writeq_atom(A):-w_out(A, writeq, 999, punct, _).

%   maybe_paren(P, Prio, Char, Ci, Co)
%   writes a parenthesis if the context demands it.

maybe_paren(P, Prio, Char, _, punct) :-
        P > Prio,
        !,
        put_code(Char).
maybe_paren(_, _, _, C, C).

%   maybe_space(LeftContext, TypeOfToken)
%   generates spaces as needed to ensure that two successive
%   tokens won't run into each other.

maybe_space(punct, _) :- !.
maybe_space(X, X) :- !,
        put_code(32).
maybe_space(quote, alpha) :- !,
        put_code(32).
maybe_space(_, _).

%   put_string(S)
%   writes a list of character codes.

put_string([]).
put_string([H|T]) :-
        put_code(H),
        put_string(T).


%   put_string(S, Q)
%   writes a quoted list of character codes, where the first
%   quote has already been written.  Instances of Q in S are doubled.

put_string([], Q) :-
        put_code(Q).
put_string([Q|T], Q) :- !,
        put_code(Q), put_code(Q),
        put_string(T, Q).
put_string([H|T], Q) :-
        put_code(H),
        put_string(T, Q).



%   w_variable(V)
%   is system dependent.  This just uses whatever Prolog supplies.

w_variable(V) :-
       fast_write(V). % seems to avoid bug with _

portray(T):-write(T).

%   w_out(Term, Style, Priority, Ci, Co)
%   writes out a Term in a given Style (display,write,writeq,print)
%   in a context of priority Priority (that is, operators with
%   greater priority have to be quoted), where the last token to be
%   written was of type Ci, and reports that the last token it wrote
%   was of type Co.

w_out(Term, _, _, Ci, alpha) :-
        var(Term),
        !,
       maybe_space(Ci, alpha),w_variable(Term).
w_out(VAR_V, Style, _, Ci, Co) :- numbervar_name(VAR_V,N), !,
        w_VAR(N, Style, Ci, Co).
w_out(N, _, _, Ci, alpha) :-
        integer(N),
        (   N < 0, maybe_space(Ci, other)
        ;   maybe_space(Ci, alpha)
        ),  !,
        fast_write(N).
w_out(Term, print, _, _Ci , alpha) :-
        portray(Term),
        !.
w_out(Atom, Style, Prio, _, punct) :-
        atom(Atom),
        current_op(P, _, Atom),
        P > Prio,
        !,
        put_code(40),
        (   Style = writeq, w_atom(Atom, Style, punct, _)
        ;   fast_write(Atom)
        ),  !,
        put_code(41).
w_out(Atom, Style, _, Ci, Co) :-
        atom(Atom),
        !,
        w_atom(Atom, Style, Ci, Co).
w_out(Term, display, _, Ci, punct) :- !,
        functor(Term, Fsymbol, Arity),
        w_atom(Fsymbol, display, Ci, _),
        w_args(0, Arity, Term, 40, display).
w_out({Term}, Style, _, _, punct) :- !,
        put_code(123),
        w_out(Term, Style, 1200, punct, _),
        put_code(125).
w_out([Head|Tail], Style, _, _, punct) :- !,
        put_code(91),
        w_out(Head, Style, 999, punct, _),
        w_tail(Tail, Style).
w_out((A,B), Style, Prio, Ci, Co) :- !,
        %  This clause stops writeq quoting commas.
        maybe_paren(1000, Prio, 40, Ci, C1),
        w_out(A, Style, 999, C1, _),
        put_code(44),
        w_out(B, Style, 1000, punct, C2),
        maybe_paren(1000, Prio, 41, C2, Co).
w_out(N, _, _, Ci, alpha) :-
        float(N),
        (   N < 0,maybe_space(Ci, other)
        ;   maybe_space(Ci, alpha)
        ),  !,
        fast_write(N).
w_out(Term, Style, Prio, Ci, Co) :-
        functor(Term, F, N),
        w_out(N, F, Term, Style, Prio, Ci, Co).

write_float(N):-fast_write(N).

w_out(1, F, Term, Style, Prio, Ci, Co) :-
        arg(1, Term, A),
        (   current_op(O, fx, F), P is O-1
        ;   current_op(O, fy, F), P = O
        ),  !,
        maybe_paren(O, Prio, 40, Ci, C1),
        w_atom(F, Style, C1, C2),
        w_out(A, Style, P, C2, C3),
        maybe_paren(O, Prio, 41, C3, Co).
w_out(1, F, Term, Style, Prio, Ci, Co) :-
        arg(1, Term, A),
        (   current_op(O, xf, F), P is O-1
        ;   current_op(O, yf, F), P = O
        ),  !,
        maybe_paren(O, Prio, 40, Ci, C1),
        w_out(A, Style, P, C1, C2),
        w_atom(F, Style, C2, C3),
        maybe_paren(O, Prio, 41, C3, Co).
w_out(2, F, Term, Style, Prio, Ci, Co) :-
        arg(1, Term, A),
        arg(2, Term, B),
        (   current_op(O, xfy, F), P is O-1, Q = O
        ;   current_op(O, xfx, F), P is O-1, Q = P
        ;   current_op(O, yfx, F), Q is O-1, P = O
        ),  !,
        maybe_paren(O, Prio, 40, Ci, C1),
        w_out(A, Style, P, C1, C2),
        w_oper(F, O, Style, C2, C3),
        w_out(B, Style, Q, C3, C4),
        maybe_paren(O, Prio, 41, C4, Co).
w_out(N, F, Term, Style, _Prio, Ci, punct) :-
        w_atom(F, Style, Ci, _),
        w_args(0, N, Term, 40, Style).


w_oper(Op, Prio, Style, Ci, Co) :-
        Prio < 700, !,
        w_atom(Op, Style, Ci, Co).
w_oper(Op, _, Style, _Ci, punct) :-
        put_code(32),
        w_atom(Op, Style, punct, _),
        put_code(32).


w_VAR(N, _Style, Ci, alpha) :-
        integer(N), N >= 0, !,
        maybe_space(Ci, alpha),
        Temp is N mod 26, Letter is Temp + 65, % $$$
        put(95), % writes "_"
        put_code(Letter),
        (   N < 26
        ;   Rest is N//26, fast_write(Rest) ), !.
w_VAR(A, _Style, Ci, Co) :-
        atom(A), !,
        maybe_space(Ci, alpha),
        w_atom(A, write, Ci, Co).
w_VAR(X, Style, Ci, punct) :-
	numbervar_name(VAR_V,X),
        w_atom('$VAR', Style, Ci, _),
        w_args(0, 1, VAR_V, 40, Style).


w_atom(('!'), _, _, punct) :- !,
        put_code(33).
w_atom((';'), _, _, punct) :- !,
        put_code(59).
w_atom([], _, _, punct) :- !,
        put_code(91), put_code(93).
w_atom('{}', _, _, punct) :- !,
        put_code(123), put_code(125).
w_atom('.', writeq, _, quote):-!,put_string("'.'").
w_atom(Atom, Style, Ci, Co) :-
        name(Atom, String),
        (   classify_name(String, Co),
            maybe_space(Ci, Co),
%$$         put_string(String)    %-- seems safe
            fast_write(Atom)
        ;   Style = writeq, Co = quote,
            maybe_space(Ci, Co),
            put_code(39), 
            put_string(String, 39)
        ;   Co = alpha, maybe_space(Ci, Co),
%$$         put_string(String)    %-- seems safe
            fast_write(Atom)
        ),  !.

%   classify_name(String, Co)
%   says whether a String is an alphabetic identifier starting
%   with a lower case letter (Co=alpha) or a string of symbol characters
%   like ++/=? (Co=other).  If it is neither of these, it fails.  That
%   means that the name needs quoting.  The special atoms ! ; [] {} are
%   handled directly in w_atom.  In a basic Prolog system with no
%   way of changing the character classes this information can be
%   calculated when an atom is created, and just looked up.  This has to
%   be as fast as you can make it.

/* Paul Tarau -> defined in read.pl: is_min, etc. */

classify_name([H|T], alpha) :-
        is_min(H),
        !,
        classify_alpha_tail(T).
classify_name([H|T], other) :-
        is_spec(H),
        classify_other_tail(T).

classify_alpha_tail([]).
classify_alpha_tail([H|T]) :-
        is_an(H),
        classify_alpha_tail(T).

classify_other_tail([]).
classify_other_tail([H|T]) :-
        is_spec(H),
        classify_other_tail(T).


%   w_args(DoneSoFar, Arity, Term, Separator, Style)
%   writes the remaining arguments of a Term with Arity arguments
%   all told in Style, given that DoneSoFar have already been written.
%   Separator is 0'( initially and later 0', .

w_args(N, N, _, _, _) :- !,
        put_code(41).
w_args(I, N, Term, C, Style) :-
        J is I+1,
        arg(J, Term, A),
        put_code(C),
        w_out(A, Style, 999, punct, _),
        w_args(J, N, Term, 44, Style).


%   w_tail(Tail, Style)
%   writes the tail of a list of a given style.

w_tail(Var, _) :-                       %  |var]
        var(Var),
        !,
        put_code(124),
        w_variable(Var),
        put_code(93).
w_tail([], _) :- !,                     %  ]
        put_code(93).
w_tail([Head|Tail], Style) :- !,        %  ,Head tail
        put_code(44),
        w_out(Head, Style, 999, punct, _),
        w_tail(Tail, Style).
w_tail(Other, Style) :-         %  |junk]
        put_code(124),
        w_out(Other, Style, 999, punct, _),
        put_code(93).

%namevars0(T,T).
%namevars0(T,T):-namevars1(T,0,_).
namevars0(V,NewV):-copy_term(V,NewV),numbervars(NewV,0,_).

add_true((H:-B),(H:-B)):-!.
add_true(H,(H:-true)).

fast_write(X):-cwrite(X).

numbervar_name(VAR_V,N):-functor(VAR_V,'$VAR',1),arg(1,VAR_V,N).


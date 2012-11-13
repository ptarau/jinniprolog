/*
% BinProlog x.xx Copyright (C) 1992-2012 Paul Tarau. 
% All rights reserved.
% COMPILER: dcgs --> prolog --> binary progs --> code
% works on a clause at a time, uses no side effects
*/

cc(Mode,C):-
	expand_term(C,E),
	fact2rule(E,R),    
	debug_hook(Mode,NewMode,R,M),
	mdef_to_def(M,D),
	def_to_mbin0(D,B),
	show_steps(NewMode,M,D),
	compile_bin(NewMode,B).

fact2rule(':-'(B),':-'(B)):-!.
fact2rule((H:-B),(H:-B)):-!.
fact2rule(H,(H:-true)).

debug_hook(debug,mem,(H:-B),(debug_clause(H,B):-true)):-!.
debug_hook(Mode,Mode,C,C).

% BINARY CLAUSE COMPILER

cc_bbuiltins(Mode):-Mode\==bin,bin_bbuiltin(B),compile_bin(Mode,B),
	fail.
cc_bbuiltins(Mode):-
	compile_bin(Mode,(bcall(Goal,_):-'$demo'(Goal))),
	fail.
cc_bbuiltins(Mode):-
	compile_bin(Mode,(bcall(_,Cont):-'$demo'(Cont))),
	fail.
cc_bbuiltins(_).

compile_bin(bin,C):-!,pp_clause(C),fail.
compile_bin(asm,C):-pp_clause('BINARY:'),pp_clause(C),cnl,fail.
compile_bin(Mode,C):-
	comp_clause(C,CodeC,Def,Exec),
	emit_code(Mode,[Def,CodeC,Exec]),
	!.
compile_bin(Mode,C):-
	errmes(failing_to_compile_clause(Mode),C).

emit_code(mem,C):-gen_code(mem,C).
emit_code(wam,C):-gen_code(wam,C).
emit_code(asm,C):-show_code(C).

show_steps(asm,M,D):-
	cnl,M\==D,pp_clause('DCG EXPANDED:'),pp_clause(M),cnl,fail
; pp_clause('DEFINITE:'),pp_clause(D),cnl,fail.
show_steps(_,_,_).
	
comp_clause(C,OCode,
	[ii(clause,'?',F1,N1),ii(firstarg,'?',G/M,LastN)],
	[ii(execute,'?',F2,N2)]):-
	add_true0(C,(H:-B)),
	firstarg(H,G/M),
	cc_h_b(H,F1/N1,B,F2/N2,RCode),
	max(N1,N2,MaxN),FirstN is MaxN+1,
	vars(RCode,OCode),
	functor(Dict,dict,MaxN),
	fill_info(OCode,Dict),
	collapse_args(Dict,1,MaxN),
	allocate_regs(OCode,FirstN/FirstN-[],FirstN/LastN-_).

cc_h_b(H,F1/N1,B,F2/N2,RCode):-
	compile_head(H,F1/N1,get-RCode,get-Rest), %pp(h=H),
	compile_body(B,F2/N2,put-Rest,put-[]),!. % pp(b=B)

firstarg(H,G/M):-arg(1,H,A),nonvar(A),!,functor(A,G,M).
firstarg(_,'_'/0).

compile_head(B,F/N)-->{bbuiltin(B,No,in_head)},!,
	{functor(B,F,N),arg(N,B,Cont)},
	emit(get,ii(bbuiltin,'?',No,Cont)).
compile_head(T,F/N)-->
	{functor(T,F,N),N>0},!,{functor(CT,F,N)},
	emit_head_top_term(N,T,CT).
compile_head(T,_)-->{errmes(unexpected_head_atom,T)}.

compile_body(Cont,'$demo'/1)-->{var(Cont)},!,
	emit_body_top_term(1,'$demo'(_),'$demo'(Cont)).
compile_body(true,true/0)-->!.
compile_body('!'(_cutp,Cont),FN)-->!,
	emit(put,ii(put,_,temp(1),_cutp)),  
	compile_body(Cont,FN).
compile_body(=(A,B,Cont),FN)-->!,
	compile_term(V1=A),
	compile_term(V2=B),
	emit(put,ii(put,_,temp(0),V1)),
	emit(put,ii(get,_,temp(0),V2)), 
	compile_body(Cont,FN).
compile_body(B,FN)-->{bbuiltin(B,No,in_body)},!,
	compile_bin_builtin(No,B,FN).
compile_body(T,F/N)-->
	{functor(T,F,N),N>0},!,{functor(CT,F,N)},
	emit_body_top_term(N,T,CT).
compile_body(T,_)-->{errmes(unexpected_body_atom,T)}.

arith_op(Op):-bbuiltin(B,arith(_,_),_),functor(B,Op,_).

% arith_no(No):-bbuiltin(_,arith(No,_),_).

% arith_outargs(K):-bbuiltin(_,arith(_,K),_).

out_reg(0,_,0).
out_reg(1,Res,Res).

compile_bin_builtin(arith(No,NbOutArgs),OpArgsCont,FN)-->!,
	{ functor(OpArgsCont,Op,N1),arg(N1,OpArgsCont,Cont),
		N is N1-1, arg(N,OpArgsCont,X), out_reg(NbOutArgs,X,Res),
		I is N-NbOutArgs,functor(NewOpArgs,Op,I) % NbOutArgs = 0,1
	},
	handle_constant_res(NbOutArgs,VarRes=Res),
	emit_top_bargs(1,I,OpArgsCont,NewOpArgs),
	emit(put,ii(arith,_Type,No,VarRes)),
	compile_body(Cont,FN).
compile_bin_builtin(No,BodyAndCont,FN)-->
	{ functor(BodyAndCont,_,N),N1 is N-1,
		arg(N,BodyAndCont,Cont),
		arg(1,BodyAndCont,Arg)
	},
	compile_bargs(N1,Arg),
	emit(put,ii(inline,_,No,_)), % inline=>void variable
	compile_body(Cont,FN).

handle_constant_res(0,_)-->!.
handle_constant_res(1,X=C)-->{var(C)},!,{X=C}.
handle_constant_res(1,X=C)-->{atomic(C)},!,
	emit(put,ii(put,_,temp(0),C)),
	emit(put,ii(get,_,temp(0),X)).
handle_constant_res(1,X=C)-->!,
	compile_term(X=C).

% handle_constant_res(1,_=C)-->{errmes(must_be_atomic_or_var,C)}.

classif_load(X,A,_)-->{var(A)},!,{X=A}.
classif_load(X,A,constant)-->{atomic(A)},!,{X=A}.
classif_load(X,A,_)-->compile_term(X=A).
	
compile_bargs(0,_)-->[].
compile_bargs(1,Arg)-->compile_term(V=Arg),
	emit(put,ii(put,_,temp(0),V)).

emit_top_bargs(I,N,_,_) --> {I>N},!.
emit_top_bargs(I,N,T,CT) --> {I=<N,I1 is I+1},
	{arg(I,T,A),arg(I,CT,X)},
	classif_load(X,A,Type),
	emit(put,ii(load,Type,I,X)),
	emit_top_bargs(I1,N,T,CT).

emit_top_bargs(I,N,T,CT) --> {I=<N},
	{arg(I,T,A),arg(I,CT,X)},
	!,
	compile_term(X=A),
	{I1 is I+1},
	emit_top_bargs(I1,N,T,CT).

emit_head_top_term(N,T,CT) --> 
	emit_top_args(get,1,1,T,CT),
	compile_arg(1,1,CT,T),
	emit_top_args(get,2,N,T,CT),
	compile_arg(2,N,CT,T).
	
emit_body_top_term(N,T,CT) --> 
	compile_arg(1,N,CT,T),!,
	emit_top_args(put,1,N,T,CT).

emit_top_args(_,I,N,_,_) --> {I>N},!.
emit_top_args(Op,I,N,T,CT) --> {I=<N},
	{arg(I,T,A),arg(I,CT,X),classif_arg(X,A,Type)}, % must be int. if const!
	!,
	emit(Op,ii(Op,Type,arg(I),X)),
	{I1 is I+1},
	emit_top_args(Op,I1,N,T,CT).

compile_term(X=T) --> {var(T)},!,{X=T}.
compile_term(X=T) --> {atomic(T)},!,{X=T}.
compile_term(X=T) --> {functor(T,F,N)},{N>0},!,
	{functor(CT,F,N)},
	emit_term(X,F,N,T,CT).

emit_term(X,F,N,T,CT) --> emit_wam(get,X,F,N,T,CT),!,compile_arg(1,N,CT,T).
emit_term(X,F,N,T,CT) --> compile_arg(1,N,CT,T),emit_wam(put,X,F,N,T,CT).

compile_arg(I,N,_,_) -->  {I>N},!.
compile_arg(I,N,CT,T) --> {I=<N},
	{arg(I,T,A),arg(I,CT,X)},
	{I1 is I+1},!,
	compile_term(X=A),
	compile_arg(I1,N,CT,T).

emit_wam(Op,X,F,N,T,CT) --> {N>0},
	emit(Op,ii(Op,structure,F/N,X)),
	emit_args(1,N,T,CT).

emit_args(I,N,_,_) --> {I>N},!.
emit_args(I,N,T,CT) --> {I=<N},
	{arg(I,T,A),arg(I,CT,X),classif_arg(X,A,Type)},
	!,
	emit(Op,ii(UnifyOp,Type,Op,X)),
	{unify_op(Op,UnifyOp),I1 is I+1},
	emit_args(I1,N,T,CT).

unify_op(put,write).
unify_op(get,unify).

classif_arg(X,A,_):-var(A),!,X=A.
classif_arg(X,A,constant):-atomic(A),!,X=A.
classif_arg(_,_,_).

emit(Mode,E,Mode-[E|Es],Mode-Es).

add_true0((H:-B),(H:-B)):-!.
add_true0(H,(H:-true)).

/*
% VARIABLE OCCURRENCE CLASSIFIER

% vars(T,R) :-
% each (selected) variable V of T gives in R a term
%
%   var(NewVar,OccNo/MaxOccurrences)
%
% and T is subject to (an ugly) side effect as selected
% variables get unified to '$OCC'(s(s(...)),MaxOccurrences)
*/
vars(T,R):-
	find_occurrences(T,R,Vars,[]),
	count_occurrences(Vars).

find_occurrences([],[])-->[].
find_occurrences([ii(Op,Type,Val,Var)|L],[ii(Op,Type,Val,Occ)|R])-->
	occurrence(Var,Occ),
	find_occurrences(L,R).

occurrence(A,A)-->{atomic(A)},!.
occurrence(V,var(NewV,1/Max))-->{var(V)},!,
	newvar(X=Max),
	{V='$OCC'(NewV,X=Max)}.
occurrence('$OCC'(OldV,X=Max),var(OldV,K/Max))-->!,
	oldvar(X=Max,K).
occurrence(Var,Occ)-->{errmes(bad_occurrence,at(var=Var,occ=Occ))}.

inc(V,K,K):-var(V),!,V=s(_).
inc(s(V),K1,K3):-K2 is K1+1,inc(V,K2,K3).

oldvar(X=_,K,Xs,Xs):-inc(X,2,K).

newvar(E,[E|Es],Es).

count_occurrences([]):-!.
count_occurrences([X=Max|Vs]):-inc(X,1,Max),count_occurrences(Vs).

/*
% ARGUMENT REGISTER OPTIMIZER

% fills Dict and and marks still free slots in variables
% with information on liftime of arguments
*/
fill_info(Is,Dict):-fill_all(Is,Dict,0,_).

tpoint(T2,T1,T2):-T2 is T1+1. % gets a time-point

fill_all([],_)-->[].
fill_all([I|Is],Dict)-->
	{fill_var_type(I)},
	fill_one(I,Dict),
	fill_all(Is,Dict).

% fills in liftime information using occurrence numbers

fill_var_type(ii(_,Type,_,var(_,Occ))):-var(Type),!,get_var_type(Occ,Type).
fill_var_type(_).

get_var_type(1/_,variable):-!.
get_var_type(K/Max,value):-K=<Max,!.

fill_one(ii(Op,constant,arg(An),_),Dict)-->!,tpoint(T),
	{mark_arg(Op,T,An,var(_-T/T,1/1),Dict)}.
fill_one(ii(_,constant,_,_),_)-->!,tpoint(_).
fill_one(ii(Op,_,arg(An),Xn),Dict)-->!,tpoint(T),
	{mark_arg(Op,T,An,Xn,Dict)},
	{mark_var(T,Xn)}.
fill_one(ii(_,_,_,var(Id,Occ)),_)-->!,tpoint(T),
	{mark_var(T,var(Id,Occ))}.

% marks the argument An of Dict with liftime information
mark_arg(get,From,An,Xn,Dict):-arg(An,Dict,Xn*_-From/_).
mark_arg(put,To  ,An,Xn,Dict):-arg(An,Dict,_*Xn-_/To).

% marks a variable with liftime information
mark_var(T,var(_-T/T,1/1)):-!.
mark_var(T,var(_-T/_,1/Max)):-1<Max,!.
mark_var(T,var(_-_/T,Max/Max)):-1<Max,!.
mark_var(_,var(_-_/_,_/_)).

% collapses arguments and variables, if possible
collapse_args(_,I,Max):-I>Max,!.
collapse_args(Dict,I,Max):-I=<Max,
	arg(I,Dict,X),
	collapse_them(I,X),
	I1 is I+1,
	collapse_args(Dict,I1,Max).

default(V1/V2):-set_to(0,V1),set_to(9999,V2).

set_to(Val,Val):-!.
set_to(_,_).

/*
% checks if argument I living ALife can be collapsed with
%   input head variable H living HLife and 
%   output body variable B living BLife
*/
collapse_them(I,var(H-HLife,_)*var(B-BLife,_)-ALife):-
	default(HLife),default(BLife),default(ALife),
	check_lifetimes(H-HLife,B-BLife,I-ALife).

check_lifetimes(I-HLife,I-BLife,I-ALife):-
	check_var_arg(HLife,ALife),
	check_var_var(HLife,BLife),
	check_var_arg(BLife,ALife),!.
check_lifetimes(I-HLife,_,I-ALife):-
	check_var_arg(HLife,ALife),!.
check_lifetimes(_,I-BLife,I-ALife):-
	check_var_arg(BLife,ALife),!.
check_lifetimes(_,_,_).

check_var_var(_/H2,B1/_):-H2=<B1,!.
check_var_var(H1/_,_/B2):-B2=<H1.

check_var_arg(X1/X2,A1/A2):-
	A1=<X1,X2=<A2.


% TEMPORARY VARIABLE ALOCATOR

allocate_regs([])-->!.
allocate_regs([I|Is])-->
	allocate_one_reg(I),
	allocate_regs(Is).

allocate_one_reg(ii(_,_,_,var(Reg-_,KMax)))-->allocate_one(Reg,KMax),!.
allocate_one_reg(_)-->[].

allocate_one(Reg,1/1)-->{var(Reg)},!,
	get_reg(Reg),
	free_reg(Reg).
allocate_one(Reg,1/Max)-->{var(Reg),1<Max},!,
	get_reg(Reg).
allocate_one(Reg,Max/Max)-->{1<Max},!,
	free_reg(Reg).

free_reg(Reg,Min/N-Regs,Min/N-[Reg|Regs]):-Reg>=Min,!.
free_reg(_,Rs,Rs).

get_reg(Reg,Min/N-[Reg|Regs],Min/N-Regs):-!.
get_reg(N,Min/N-Regs,Min/N1-Regs):-N1 is N+1.


/*
%----------PREPROCESSOR TO BINARY LOGIC PROGRAMS-------------

% Transforms definite metaprograms to binary meta-programs
% definite meta clause -> definite clause+ some replacements
*/
mdef_to_def((H:-B),(H:-NewB)):-replace_body(B,NewB).
mdef_to_def(':-'(B),':-'(NewB)):-replace_body(B,NewB),!,
	( NewB,  % called here and not propagated to the next step
		fail
	).
	
% replaces metavars and some bbuiltins in clause bodies
			 
replace_body(MetaVar,'$demo'(MetaVar)):-var(MetaVar),!.
replace_body('!','!'(X)):-!,cutp(X).
replace_body(var(X),Known):-nonvar(X),!,replace_known(var(X),Known).
replace_body(nonvar(X),Known):-nonvar(X),!,replace_known(nonvar(X),Known).
replace_body(integer(X),Known):-nonvar(X),!,replace_known(integer(X),Known).
replace_body(atomic(X),Known):-nonvar(X),!,replace_known(atomic(X),Known).
replace_body((A,B),NewAB):-nonvar(A),split_op(A,NewA),!,
	replace_body(B,NewB),
	app_body(NewA,NewB,NewAB).
replace_body((A,B),(NewA,NewB)):-!,
	replace_body(A,NewA),
	replace_body(B,NewB).
replace_body((A->B;C),if(NewA,NewB,NewC)):-!,
	replace_body(A,NewA),
	replace_body(B,NewB),
	replace_body(C,NewC).
replace_body((A;B),or(NewA,NewB)):-!,
	replace_body(A,NewA),
	replace_body(B,NewB).
replace_body((A->B),(NewA->NewB)):-!,
	replace_body(A,NewA),
	replace_body(B,NewB).
replace_body(compare(R,A,B),compare0(A,B,R)):-!.
replace_body(A==B,compare0(A,B,=)):-!.
replace_body(A@<B,compare0(A,B,<)):-!.
replace_body(A@>B,compare0(A,B,>)):-!.
replace_body(A,NewA):-split_op(A,L),!,
	strip_nil(L,NewA).
replace_body(A,A).

replace_known(X,true):-X,!.
replace_known(_,fail).
	
split_op(X is B,R):-split_is_rel(X,B,R).
split_op(A < B,R):-split_rel(<,A,B,R).
split_op(A > B,R):-split_rel(>,A,B,R).
split_op(A =< B,R):-split_rel(=<,A,B,R).
split_op(A >= B,R):-split_rel(>=,A,B,R).
split_op(A =:= B,R):-split_rel(=:=,A,B,R).
split_op(A =\= B,R):-split_rel(=\=,A,B,R).

split_is_rel(X,B,[+(B,0,X)]):-var(B),!.
split_is_rel(X,B,[+(B,0,X)]):-atomic(B),!.
split_is_rel(X,B,R):-split_is(X,B,R,[]).

app_body([],Bs,Bs):-!.
app_body([A|As],Bs,(A,Cs)):-app_body(As,Bs,Cs).

strip_nil([A],A):-!.
strip_nil([A|As],(A,Bs)):-strip_nil(As,Bs).

split_rel(Op,A,B,Res):-split_rel_1(Op,A,B,Res,[]).

split_rel_1(Op,A,B)-->
	split_is(X,A),
	split_is(Y,B),
	{OpXY=..[Op,X,Y]},
	emit_is(OpXY).

split_is(X,A)-->{var(A)},!,{X=A}.
split_is(X,A)-->{atomic(A)},!,{X=A}.
split_is(X,+(A)) --> !,split_is(X,A).
split_is(X,-(A)) --> !,split_is(X,0-A).
split_is(X,\(A)) --> !,split_is(X, \(0,A)).
split_is(R,OpAB)-->
	{OpAB=..[Op,A],math_op1(Op)},
	!,
	split_is(VA,A),
	{OpArgs=..[Op,VA,R]},
	emit_is(OpArgs).
split_is(R,OpAB)-->
	{OpAB=..[Op,A,B],(arith_op(Op);math_op2(Op))},
	!,
	split_is(VA,A),
	split_is(VB,B),
	{OpArgs=..[Op,VA,VB,R]},
	emit_is(OpArgs).

	
emit_is(X,[X|Xs],Xs).

/*
% converts a definite clause to a binary metaclause
%    where each metavariable Cont represents a "continuation"
%    and a goal G is represented by a clause :- G.
*/
% def_to_mbin0((:-B),(:-BC)):-!,add_cont0(B,true,BC).
def_to_mbin0((H:-true),(HCont:-'$demo'(Cont))):-!,
		 add_last_arg(H,Cont,HCont).
def_to_mbin0((H:-B),(HC:-BC)):-!,
		 add_last_arg(H,Cont,HC),
		 add_cont0(B,Cont,BC).
def_to_mbin0(H,(HCont:-'$demo'(Cont))):-!,
		 add_last_arg(H,Cont,HCont).

% adds a continuation to a term

add_cont0((true,Gs),C,GC):-!,add_cont0(Gs,C,GC).
add_cont0((fail,_),C,fail(C)):-!.
add_cont0((G,Gs1),C,GC):-!,
		 add_cont0(Gs1,C,Gs2),
		 add_last_arg(G,Gs2,GC).
add_cont0(true,C,'$demo'(C)):-!.
add_cont0(fail,C,fail(C)):-!.
add_cont0(G,C,GC):-
		 add_last_arg(G,C,GC).
		 
% appends a last argument to a term

add_last_arg(Term,Cont,TermAndCont):-
		 functor(Term,F,N),
		 N1 is N+1,
		 functor(TermAndCont,F,N1),
		 arg(N1,TermAndCont,Cont),
		 copy_args(1,N,Term,TermAndCont).

% copies the arguments of a term to a new term

copy_args(I,Max,_,_):-I>Max,!.
copy_args(I,Max,Term,TermAndCont):-I=<Max,!,
		 arg(I,Term,X),
		 arg(I,TermAndCont,X),
		 I1 is I+1,
		 copy_args(I1,Max,Term,TermAndCont).

% simple WAM-code lister

show_code(IIs):-
	pp('WAM-ASSEMBLER:'),
	member(Is,IIs),
	member(I,Is),
	show_or_skip(I),
	fail.
show_code(_):-cnl.

show_or_skip(ii(get,variable,arg(I),var(I-_,_))):-!.
show_or_skip(ii(put,value,arg(I),var(I-_,_))):-!.
show_or_skip(ii(Op,T,X,Y)):-
	write_unquoted(Op),
	write_unquoted('_'),
	write_unquoted(T),
	write_unquoted(' '),
	write_unquoted(X),
	write_unquoted(' '),
	write_unquoted(Y),
	nl.
	
% BYTE CODE GENERATOR

% eliminates redundancies and beautifies the code
beautify(arg(X),Op,Type,V,To):-!,encode_arg(X,Op,Type,V,To).
beautify(temp(X),Op,Type,V,To):-!,encode_arg(X,Op,Type,V,To).
beautify(Val,Op,Type,var(Xn-_,_),To):-!,encode2(Op,Type,Val,Xn,To).
beautify(put,write,constant,X,To):-cutp(X),!,encode2(push,cut,'?','?',To).
beautify(Y,Op,X,Z,To):-encode2(Op,X,Y,Z,To).

encode_arg(I,get,variable,var(I-_,_),_):-!.
encode_arg(I,put,value,var(I-_,_),_):-!.
encode_arg(An,Op,Type,var(Xn-_,_),To):-!,encode2(Op,Type,Xn,An,To).
encode_arg(1,Op,constant,X,To):-cutp(X),!,encode2(Op,cut,'?','?',To).
encode_arg(An,Op,constant,S,To):-encode2(Op,constant,S,An,To).

encode2(Op,Type,X,Y,To):-
	symcat(Op,Type,OpType),
	encode(OpType,X,Y,To).

encode(unify_variable,get,Ai,To):-        wcode(To,1,Ai,'?',0).
encode(write_variable,put,Ai,To):-        wcode(To,2,Ai,'?',0).

encode(unify_value,get,Ai,To):-           wcode(To,3,Ai,'?',0).
encode(write_value,put,Ai,To):-           wcode(To,4,Ai,'?',0).

encode(unify_constant,get,Const,To):-     wcode(To,5,0,Const,0).
encode(write_constant,put,Const,To):-     wcode(To,6,0,Const,0).

encode(get_constant,Const,Ai,To):-        wcode(To,7,Ai,Const,0).
encode(get_structure,Func/Arity,Ai,To):-  wcode(To,8,Ai,Func,Arity).

encode(put_constant,Const,Ai,To):-        wcode(To,9,Ai,Const,0).
encode(put_structure,Func/Arity,Ai,To):-  wcode(To,10,Ai,Func,Arity).

encode(get_variable,Xn,Ai,To):-           wcode(To,11,Xn,'?',Ai). %move_reg
encode(put_value,Xn,Ai,To):-              wcode(To,11,Ai,'?',Xn). %move_reg

encode(put_variable,Xn,Ai,To):-           wcode(To,12,Xn,'?',Ai).
encode(get_value,Xn,Ai,To):-              wcode(To,13,Xn,'?',Ai).

encode(push_cut,'?','?',To):-                 wcode(To,14,0,'?',0).
encode(put_cut,'?','?',To):-                  wcode(To,15,0,'?',0).
encode(get_cut,'?','?',To):-                  wcode(To,16,0,'?',0).

encode('execute_?',Pred,Arity,To):-       wcode(To,17,0,Pred,Arity).
% proceed ==> 18 (by emulator)

encode(load_constant,AReg,Const,To):-     wcode(To,28,AReg,Const,0).
encode(load_variable,AReg,V,To):-         wcode(To,29,AReg,'?',V).
encode(load_value,AReg,V,To):-            wcode(To,29,AReg,'?',V).

encode('clause_?',Pred,N,To):-            wcode(To,254,0,Pred,N).
encode('firstarg_?',F/N,MaxRegs,To):-     wcode(To,255,MaxRegs,F,N).

encode('end_?',Reg,Mode,To):-             wcode(To,0,Reg,Mode,0).

encode(inline_variable,No,_Arg,To):-n_binline(N),  wcode(To,N,0,'?',No).

encode(arith_variable,No,Arg,To):-n_barith(N),   wcode(To,N,Arg,0,No).
encode(arith_value,No,Arg,To):-n_barith(N),      wcode(To,N,Arg,1,No).

encode('bbuiltin_?',No,Arg,To):-n_bbuiltin(N),    wcode(To,N,Arg,'?',No).

wcode(mem,Op,Reg,F,N):-   % cwrite(ii(Op,Reg,F,N)),cnl,
   add_instr(0,Op,Reg,F,N). % @@ a good place to trace code generation
wcode(wam,Op,Reg,F,N):-
   add_instr(1,Op,Reg,F,N).
 
gen_instr(ii(Op,Type,Arg,Var),To,yes):-beautify(Arg,Op,Type,Var,To),!.
gen_instr(_,_,no).

gen_code(To,IIs):-
	member(Is,IIs),member(I,Is),gen_instr(I,To,Ok),
	Ok=no,
	!,
	fail.
gen_code(_,_).


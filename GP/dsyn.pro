/* simple synthetizers, try:

?-syn((A*B)+(~C)).

?-syn(3:29). % <number of vars>:<truth table as integer>

*/

syn(E):-syn([<, =>],[0,1],E).

ssyn(E):-syn([nand,nor],[0,1],E).

syn(Fs,E):-syn(Fs,[],E).

syn(Fs,Cs,E):-expr2tt(E,NV:TT),syn(Fs,Cs,NV,TT).

expr2tt(NV:TT,NV:TTs):-integer(NV),!,to_list(TT,TTs).
expr2tt((Vs:E),NV:TT):-eval_expr(Vs,E,NV,TT).
expr2tt(E,NV:TT):- %vars_of(E,Vs),
  Vs=[],eval_expr(Vs,E,NV,TT).

syn(Fs,Cs,NV,TT):-
  MG is NV*(1<<NV),
  to_list(TT,TTs),
  syn(NV,MG,Fs,Cs,TTs).

% synthetize and print result
syn(NV,MG,Fs,Cs,TTs):-
  portray_clause(syn(NV,MG,Fs,Cs,TTs)),
  ( member(TT,TTs),
      show_tt(NV,TT),
    fail
  ; true
  ),
  runtime(T1),
  syn(NV,MG,Fs,Cs,TTs, R),
  !,
  runtime(T2),
  portray_clause(R),
  T is T2-T1,
  write(time_ms=T),nl,
  fail.

% synthetise and show in a more readable form

syn(NV,MG,Fs,Cs,TTs, Xs:Gs=Ys:TTs):-
  syn(NV,MG,Fs,Cs,TTs, Xs,Gs,Ys).

% synthetiser API - Fs=lib,Cs=consts,E=expr
% output: TTs=ttable, Xs=invars,Gs=DAG,Ys=outvars
syn(Fs,Cs,E, TTs, Xs,Gs,Ys):-
  expr2tt(E,NV:TT),to_list(TT,TTs), 
  MG is NV*(1<<NV),
  syn(NV,MG,Fs,Cs,TTs, Xs,Gs, Ys).

syn(NV,MG,Fs,Cs, TTs, Xs,Gs,Ys):-
  vGS(NV,MG,Fs,Cs,TTs,Vs,Gs0),
  !,
  showdag(NV,Cs,Vs,Gs0, Xs,TTs,Ys, Gs).

% given: 
%   NV variables, MG max gates, constants Cs, functions Fs,
%   a list of output truth table vectors TTs
% => build a DAG as:
%   Vs, a list of primary input variables
%   Frees, a list of gates forced to cover outputs
%   Bound, a list of gates used internally or for outputs
%   Os a list of output variables

vGS(NV,MG,Fs,Cs,TTs, Vs,Gs):-
  nonvar(TTs),length(TTs,L),L<6,
  iIs(NV,Cs, M,Vs,Is),
  (member(T,TTs),member(T,Is)->fail;true),
  !,
  findall(Os,operm(TTs,_End,Os),Oss),
  aGS(Oss,MG,M,Fs,Is,no_gs, Gs).
vGS(NV,MG,Fs,Cs,TTs, Vs,Gs):-
  genGS(NV,MG,Fs,Cs,TTs, Vs,Gs).

genGS(NV,MG,Fs,Cs,TTs, Vs,Gs):-
  iIs(NV,Cs, M,Vs,Is),
  eGS(MG,M,Fs,Is, Gs,Os),
  distinct_members(TTs,Os).

distinct_members([],_).
distinct_members([T|Ts],Os):-
  sel(T,Os,NewOs),
  distinct_members(Ts,NewOs).
  
aGS([],_MG,_M,_Fs,_Is, Gs,Gs).
aGS([Os|Oss],MG,M,Fs,Is, Gs1,Gs2):-
  ( eGS(MG,M,Fs,Is, Gs0,Os),
    length(Gs0,NG0),length(Is,L),NG1 is NG0-L,
    NG1=<MG
  ->[NG,Gs]=[NG1,Gs0]
  ; [NG,Gs]=[MG,Gs1]
  ),
  aGS(Oss,NG,M,Fs,Is, Gs,Gs2).
  
% generates gates
eGS(_,_,_,Is, Is,Is).
eGS(N,M,Fs,Is, [G|Gs],[O|Os]):-
  N>0,N1 is N-1,
  eGS(N1,M,Fs,Is, Gs,Os),
  eADD(Fs,M,Os,G,O).

% add a gate, connect it and evaluate it
eADD(Fs,M,Os, g(F,VI,VJ,VO),VO):-
  member(VI,Os),
    member(VJ,Os),
      member(F,Fs),
        applyF(F,M,VI,VJ,VO),
           (member(VO,Os)->fail;true).

iIs(NV,Cs, Mask,Vs,Is):-
 init_vs(NV,Mask,Vs),
 init_cs(Cs,Mask,ICs),
 append(ICs,Vs,Is).
    
% constant mapping
const(0,_M,0). % false=0
const(1,M,M).  % true=M (Mask)

% precompute constants
init_cs([],_,[]).
init_cs([C|Cs],M,[VC|ICs]):-const(C,M,VC),init_cs(Cs,M,ICs).

% precomputes bitvector values for variables
init_vs(NV,Mask,VPairs):-
  all_ones_mask(NV,Mask),
  vars_to_bitstring_ints(NV,VPairs).
  
% removes unneeded information - for more readable gates
showdag(NV,Cs,VVs,Gs, Vs,Os,Ys,NewGs):-
  simplify_consts(NV,Cs,D),
  simplify_list(VVs,Vs,D),
  simplify_list(Os,Ys,D),
  reverse(Gs,Rs),
  simplify_gs(Rs,NewGs,D).

simplify_consts(NV,Cs,D):-
  all_ones_mask(NV,M),
  init_cs(Cs,M,As),simplify_list(As,Cs,D).

% max bitstring int of the form 11...1 build with NbOfBits bits 
all_ones_mask(NbOfBits,Mask):-Mask is (1<<(1<<NbOfBits))-1. 

% bind N free vars to their N-bit bitstring_int representation
vars_to_bitstring_ints(NbOfBits,Vs):-
  vars_to_bitstring_ints(NbOfBits,0,NbOfBits,Vs).

vars_to_bitstring_ints(_,N,N,[]).
vars_to_bitstring_ints(NbOfBits,N1,N2,[X|Xs]):-
  N1<NbOfBits,
  N is N1+1,
  var_to_bitstring_int(NbOfBits,N1,X),
  vars_to_bitstring_ints(NbOfBits,N,N2,Xs).

% var in 0..k to truth table seen as int - faster
% uses trick from Knuth 2006 - Boolean Evaluation
var_to_bitstring_int(NbOfBits,K,Xk):-
  all_ones_mask(NbOfBits,Mask),
  NK is NbOfBits-(K+1),
  D is (1<<(1<<NK))+1,
  Xk is Mask//D.
  
% bitstring int operations
% can also be seen as f:[0..M]x[0..M]->[0..M]
% or f:[0..3]->[0..1] or f<-[0..15] using their tt

applyF('nand',M,X1,X2,X3):-X3 is xor(M,/\(X1,X2)).
applyF('nor',M,X1,X2,X3):-X3 is xor(M,\/(X1,X2)).
applyF('<',_,X1,X2,X3):-X3 is xor(X1,\/(X1,X2)). %k
applyF('>',_,X1,X2,X3):-X3 is xor(X1,/\(X1,X2)). %k
applyF('=>',M,X1,X2,X3):-X3 is \/(xor(M,X1),X2).
applyF('<=',M,X1,X2,X3):-X3 is \/(X1,xor(M,X2)).
applyF('*',_,X1,X2,X3):-X3 is /\(X1,X2). %k
applyF('+',_,X1,X2,X3):-X3 is \/(X1,X2). %k
applyF('=',M,X1,X2,X3):-X3 is xor(M,xor(X1,X2)).
applyF('^',_,X1,X2,X3):-X3 is xor(X1,X2). %k
applyF('head',_,X1,_,X3):-X3 is X1.
applyF('tail',_,_,X2,X3):-X3 is X2.
applyF('nhead',M,X1,_,X3):-X3 is xor(M,X1).
applyF('ntail',M,_,X2,X3):-X3 is xor(M,X2).
applyF('zero',_,_,_,0).
applyF('one',M,_,_,M).

cm(X):-[CM,SP]="% ",ttyput(CM),ttyput(SP),ttyprint(X).



libstat:-
  tell('data.csv'),libstat1,told,
  tell('data.pl'),listing,told.

libstat1:-
  MG=4, %4,5
  MV=4, %4
  MaxLibOps=3, %3
  MaxLibGates=3, %3
  MaxSyn16=6, %6
  cm(['MG'=MG,'MV'=MV,'MaxLibOps'=MaxLibOps,
      'MaxLibGates'=MaxLibGates,syn16=MaxSyn16]),
  cm(['Ops','Consts','NumVars','NumGates','TT Vals Covered']), 
  (
    Fs=[nand],Cs=[0]
  ; Fs=[nand],Cs=[1]
  ; Fs=[nor],Cs=[0]
  ; Fs=[nor],Cs=[1]
  ; Fs=[nand,nor],Cs=[]
  ; Fs=[nand,nor],Cs=[0,1]
  ; Fs=[<,=>],Cs=[0,1]
  ; Fs=[*,^],Cs=[0,1]
  ; Fs=[*,=],Cs=[0,1]
  ; cm('minimal universal libs'),fail
  ; ulib(MaxLibOps,MaxLibGates,Fs,Cs)
  ),
  ( % meant to fail un uninteresting libs
    syn16(MaxSyn16,Fs,Cs,Res)->
    cm(Fs+Cs=succeeds(MaxSyn16)),
    assert(s16(MaxSyn16,Fs,Cs,Res))
  ; cm(Fs+Cs=fails(MaxSyn16)),
    assert(s16(MaxSyn16,Fs,Cs,0)),
    fail
  ),
  between(1,MG,XG),
      between(1,MV,NV),
        scov(NV,MG,Fs,Cs, NG,R),
        Data=[Fs,Cs,NV,XG,NG,R,Res],
        to_prolog_fact(Data),
        to_cdelim(Data),
  fail
; true.

uls:-
  tell('uls.pl'),
  new_ctr(Ctr),
  (
     MaxLibOps=3, %3 - running at 4 proves no larger minimal libs
     MaxLibGates=3, %3
     ulib(MaxLibOps,MaxLibGates,Fs,Cs),
       inc_ctr(Ctr,1),
       get_ctr(Ctr,K),
       T=ul(K,Fs,Cs),
       ttyprint(T),
       portray_clause(T),
     fail
  ; true
  ),
  told.
  
to_prolog_fact(Args):-T=..[sc|Args],assert(T).

to_cdelim([Fs,Cs,NV,XG,NG,R,S16]):-
  [UL,CM,Q]=",,""",
  ( put(Q),
    nth_member(F,Fs,NF),
      (NF>1->put(UL);true),
      write(F),
    fail
  ; member(C,Cs),
      put(UL),
      write(C),
     fail
  ; put(Q),put(CM),
      write(NV),put(CM),write(XG),
      put(CM),write(NG),put(CM),write(R),put(CM),write(S16)
  ; nl
  ).
  
scov(MG):-scov(3,MG,[<,=>],[],NG,R),write(NG+R),nl.

ccovs:-
  ulib(Fs,Cs),
  between(0,2,NV),
  ccov(Fs,NV,Rs),
  println(NV:Fs+Cs=Rs),
  fail.

scov(NV,MG):-scov(NV,MG,[<,=>],[],NG,R),write(NG+R),nl.

sscov(NV,MG):-scov(NV,MG,[nand,nor],[],NG,R),write(NG+R),nl.
  
% test how much a given library Fs covers
scov(NV,MG,Fs,Cs, NG,R):-
 Ctr=s(0),
 iIs(NV,Cs, Max,_Vs,Is),
 (
   between(0,Max,I),
   val(tt,I,1),rm(tt,I),
   fail
 ; 
   eGS(MG,Max,Fs,Is,Gs,[TT|_Os]),
   ( val(tt,TT,1)->true
   ; let(tt,TT,1),
     arg(1,Ctr,Cov),
     Cov1 is Cov+1,
     change_arg(1,Ctr,Cov1)
   ),
   arg(1,Ctr,Cov),
   ( Cov>Max->!,R=Cov,length(Gs,NGV),NG is NGV-NV
   ; fail
   )
 ; arg(1,Ctr,R),NG=MG
 ).

scov1(NV,MG,Fs,Cs,TT, Gs):-
  iIs(NV,Cs, Max,_Vs,Is),
  eGS(MG,Max,Fs,Is,Gs,[TT|_Os]).

% synthetiser for 4 key classes of 2-arg functions

syn16all(MG,Fs,Cs,Xs+Gs+Ys):-
  NV=2,
  %     [*,>,<,^,+]
  % TTs=[1,2,4,6,7],
  %   [*,>,^,+]
  TTs=[1,2,6,7],
  % findall(I,between(0,15,I),TTs),
  syn(NV,MG,Fs,Cs, TTs, Xs,Gs,Ys).

syn16(0,_Fs,_Cs,Result):-!,Result=0.
syn16(MG,Fs,Cs,Result):-
  NV=2,
  length(Cs,L0),
  iIs(NV,Cs, Max,_Vs,Is),
  new_ctr(Sum),
  ( between(0,15,TT),
      ( member(TT,Is)->true
      ; eGS(MG,Max,Fs,Is,Gs,[TT|_Os])->
        length(Gs,L1),
        L is L1-(NV+L0),
        %write(L),put(32),portray_clause(Gs),
        inc_ctr(Sum,L)
      ; % println(failing(TT)=Is),
        !
      ),
    fail
  ; get_ctr(Sum,Result)
  ).
     

% counters persisting on backtracking
new_ctr(s(0)).
    
inc_ctr(Ctr,Inc):-
  arg(1,Ctr,Old),
  New is Old+Inc,
  change_arg(1,Ctr,New).

get_ctr(Ctr,Val):-arg(1,Ctr,Val).
   
% minimal universal library enumerator
ulib:-
  ulib(Fs,Cs),
  write(Fs+Cs),nl,
  fail
; nl.

ulib(Fs,Cs):-
  MaxOps=3,
  MaxGates=3,
  ulib(MaxOps,MaxGates,Fs,Cs).

ulib(MaxOps,MaxGates,Fs,Cs):-
  findall(Op,(op2code(Op,_)
     %,Op\==zero,Op\==one,Op\==ntail
  ),Ops),
  between(0,MaxOps,N),
    ksubset(N,Ops,Fs),
      maybelib([0,1],Cs),
        ulib1(MaxGates,Fs,Cs).
  
ulib1(MaxGates,Fs,Cs):- 
  is_univ(MaxGates,Fs,Cs),
  ( sublib(Fs,Cs,Fs1,Cs1),
    is_univ(MaxGates,Fs1,Cs1),
    !,
    fail
  ; true
  ).
  
allops(Ops):-
  findall(Op-Code,op2code(Op,Code),Ops).

op2code(Op,Code):-init_vs(2,M,[V0,V1]),applyF(Op,M,V0,V1,Code).

is_univ(MG,Fs,Cs):-is_univ(MG,Fs,Cs,_,_,_).

is_univ(MG,Fs,Cs, Op,NG,Gs):-
  scov1(2,MG,Fs,Cs,Z,Gs),
  member(Op-Z,[nand-14,nor-8]),
  !,
  length(Gs,LG),length(Cs,LC),
  NG is LG-(LC+2).

% library generator
maybelib(Ops,Lib):-
  length(Ops,L),
  between(0,L,I),
    ksubset(I,Ops,Lib).

ksubset(0,_,[]).
ksubset(K,[X|Xs],[X|Rs]):-K>0,K1 is K-1,ksubset(K1,Xs,Rs).
ksubset(K,[_|Xs],Rs):-K>0,ksubset(K,Xs,Rs).

sublib(Fs,Cs,Fs1,Cs1):-
   length(Fs,LF),
   length(Cs,LC),
   LF1 is LF-1,
   LC1 is LC-1,
   ( ksubset(LF1,Fs,Fs1),ksubset(LC1,Cs,Cs1)
   ; Cs1=Cs,ksubset(LF1,Fs,Fs1)
   ; Fs1=Fs,ksubset(LC1,Cs,Cs1)
   ).
 
/*  
add_ops([],Cs,Cs).
add_ops([Op-I|Xs],Cs,T):-add_op(Op,I,T,NewT),add_ops(Xs,Cs,NewT).

add_op(Op,I,T,NewT):-I1 is I+1,functor(T,a,16),arg(I1,T,v(Op,NewT)).
*/
 
 % tests how many different inputs lead to a given output value
ccov(Ops,NV):-
  ccov2(Ops,NV,TT,R),
  write(TT:R),
  nl,
  fail
; nl.

% list of ccov2 coverages: sum always == 2^(2^NV)
ccov(Ops,NV,Rs):-
  findall(R,ccov2(Ops,NV,_TT,R),Rs).

% coverage of all TT in 0..Max with ccov1  
ccov2(Ops0,NV,TT,R):-
  to_list(Ops0,Ops),
  all_ones_mask(NV,Max),
  between(0,Max,TT),
  ccov1(Ops,NV,TT,R).

% how many pairs I,J such that exists Op in Ops, Op(I,J)==T   
ccov1(Ops,NV,TT, R):-
  all_ones_mask(NV,Max),
  C=s(0),
  ( between(0,Max,I),
      between(0,Max,J),
        member(Op,Ops),
          applyF(Op,Max,I,J,TT),
          arg(1,C,X),
          X1 is X+1,
          change_arg(1,C,X1),
    fail
  ; true 
  ),
  arg(1,C,R).

/*
ite_syn(TT):-
   NV=2,
   Is=[_,_,0,1],
   ksubset(2,Is,[X,Y]),
   eval_expr([X,Y,Z],(U=ite(X,Y,Z)),NV,TT).
*/
   
% expression evaluator - supports all 16 binary ops, ~,ite

eval_expr(Vs0,E,NV,I):-
  to_list(E,Es0),
  copy_term(Vs0+Es0,Vs+Es),
  numbervars(Vs+Es,0,NV),
  all_ones_mask(NV,M),
  mapeval(Es,NV,M,R),
  !,
  R=I.

mapeval([],_,_,[]).
mapeval([E|Es],NV,M,[R|Rs]):-
   eval_one(E,NV,M,R),
   mapeval(Es,NV,M,Rs).

eval_one(E,_,M,I):-integer(E),!,
  const(E,M,I).
eval_one('$VAR'(K),NV,_M,I):-!,
  var_to_bitstring_int(NV,K,I).
eval_one(E,NV,M,I):-functor(E,F,2),!,
  arg(1,E,X),arg(2,E,Y),
  eval_one(X,NV,M,A),eval_one(Y,NV,M,B),
  applyF(F,M,A,B,I).
eval_one(~(E),NV,M,I):-!,
  eval_one(E,NV,M,A),
  I is xor(M,A).
eval_one(ite(X,Y,Z),NV,M,I):-!,
  eval_one(((X*Y)+(~(X)*Z)),NV,M,I).

% tools

% selects a value from a list
sel(X,[X|Xs],Xs).
sel(X,[Y|Xs],[Y|Ys]):-sel(X,Xs,Ys).
    
sufsel(X,Xs,Ys):-append(_,[X|Ys],Xs).

operm(Ts,End,Os):-perm(Ts,Ps),append(Ps,End,Os).

perm([],[]).
perm([X|Xs],Zs):-
	perm(Xs,Ys),
	sel(X,Zs,Ys).
	
to_list(Es,R):-nonvar(Es),Es=[_|_],!,R=Es.
to_list([],R):-!,R=[].
to_list(E,[E]).

show_tt(NV,Int):-
  show_tt(NV,Int,BsV),
  write(BsV),nl,
  fail
; nl.
show_tt(NV,Int,Bs:V):-
  findall(Bs,tt_line(NV,Bs),Bss),
  T=..[tt|Bss],
  functor(T,_,N),
  between(1,N,I),
  arg(I,T,Bs),
  I1 is N-I,
  getbit(Int,I1,V).
  
tt_line(0,[]).
  tt_line(N,[B|Bs]):-N>0,N1 is N-1,(B=0;B=1),tt_line(N1,Bs).

getbit(Int,Bit,Val):- Val is (/\(Int,(1<<Bit)))>>Bit.

to_var(C,X,D):-member(v(X,C),D),!.

simplify_list([],[],_).
simplify_list([C|Cs],[X|Xs],D):-to_var(C,X,D),simplify_list(Cs,Xs,D).

simplify_gs([],[],_).
simplify_gs([g(Op,_Mask,A,B,C)|Gs],[T|Ts],D):-!,
  simplify_list([A,B,C],[X,Y,Z],D),
  T=..[Op,X,Y,Z],
  simplify_gs(Gs,Ts,D).
simplify_gs([g(Op,A,B,C)|Gs],[T|Ts],D):-!,
  simplify_list([A,B,C],[X,Y,Z],D),
  T=..[Op,X,Y,Z],
  simplify_gs(Gs,Ts,D).
simplify_gs([_C|Gs],Ts,D):-
  simplify_gs(Gs,Ts,D).
  
  
% tests

bm:-
  runtime(T1),
  bm1,
  runtime(T2),
  T is T2-T1,
  write(time=T),nl.
    
bm1:-  
  between(0,16,I),
  % namecat(t,'',I,G),
  [T]="t",number_codes(I,Is),atom_codes(G,[T|Is]),
  nl,write('-----'),write(G),write('-----'),nl,
  call(G),
  fail.
bm1.

runtime(T):- statistics(runtime,[T,_]).
 
go:-
 Fs=['<','=>'],
 NV=2,
 MG=6,
 Cs=[],
 TTs=[6,1], % half adder
 Goal=syn(NV,MG,Fs,Cs,TTs),
 call(Goal).

ngo:- % half adder with nand
 syn([nand],[],2:[6,1]),fail
;syn([nand],[],[A^B,A*B]).
  
test:-
 Fs=['<'],
 NV=2,
 MG=3,
 Cs=[1],
 TTs=[1],
 Goal=syn(NV,MG,Fs,Cs,TTs),
 call(Goal).

:-op(700,xfx,'=>').

% easy examples

t0:-syn([],[0,1],2:15).

t1:-syn(['=>','<'],[],2:6).

t2:-syn(['=>'],[0],2:1).
t3:-syn(['<'],[1],2:9).

t4:-syn(['nand','nor'],[],2:6).

t5:-syn(['nand'],[],2:6).
t6:-syn(['nor'],[],2:6).

t7:-syn(['=>','<'],[0,1],2:9).
t8:-syn(['nand','nor'],[],2:6).

t9:-syn(['=>','<'],[],3:71).  % ite
t10:-syn(['nand','nor'],[],3:71).

t11:-syn(['=>','<'],[],3:29). % ite01
t12:-syn(['nand','nor'],[],3:29).

t13:-syn(['=>','<'],[0,1],3:71).
t14:-syn(['nand','nor'],[0,1],3:71). 

t15:-syn(['=>','<'],[0,1],3:29).
t16:-syn(['nand','nor'],[0,1],3:29).

% multi output

m1:-syn(2,6,['=>','<'],[],[6,9]).
m2:-syn(2,6,['nand','nor'],[],[6,9]). % A^B,A=B

m3:-syn(2,6,['=>','<'],[],[6,1]). % half adder
m4:-syn(2,6,['nand','nor'],[],[6,1]). % half adder

m5:-syn(2,6,['nand'],[],[6,1]). % half adder
m6:-syn(2,6,['nor'],[],[6,1]). % half adder

m7:-syn(2,6,['<'],[1],[6,1]). % half adder

m8:-syn(2,8,['=>'],[0],[6,1]). % half adder
m9:-syn(3,6,['=>','<'],[],[105]). % A^B^C
/*
?- m9.
syn(3,6,[=>,<],[],[A^B^C]).
[0,0,0]:0
[0,0,1]:1
[0,1,0]:1
[0,1,1]:0
[1,0,0]:1
[1,0,1]:0
[1,1,0]:0
[1,1,1]:1

[A,B,C]: [
    =>(A,B,D),
    <(A,B,E),
    <(E,D,F),
    =>(F,C,G),
    <(F,C,H),
    <(H,G,I)
] = [_I]:[105].
*/

m10:-syn([<,=>],[0,1],[A*B+B*C+A*C, A^B^C]). % full adder
/*
[A,B,C]:[
  =>(A,0,D),
  <(D,B,E),
  =>(D,B,F),
  =>(F,E,G),
  <(C,G,H),
  =>(C,G,I),
  =>(I,E,J),
  <(H,I,K)
] = [J,K]:[23,105].
*/

/*
?- m10a.
[_A,_B,_C]:[=>(_A,_A,_D),<(_A,_D,_E),=>(_E,_B,_F),=>(_B,_E,_G),<(_C,_G,_H),<(_H,_F,_I)] = [o(_I,23)].
*/
m10a:-syn(3,8,['=>','<'],[],[23]). % carry on in full adder
m10x:-syn(3,8,['*','^'],[1],[23,105]). % full adder ?

m10e:-syn([<,=],[],[A*B+B*C+A*C,A^B^C]). % full adder

m11:-syn(3,8,['=>','<'],[],[15,51,85]). % id

m12:-syn(3,8,['=>','<'],[],[15,85,51]). % swap

m13:-syn(3,8,['=>','<'],[],[15,85,51]). % swap

m14:-syn(3,8,['=>','<'],[],[15,53,85]).

% Fredkin gate variant - Wikipedia ?? 3x3

m15:-syn(3,8,['=>','<'],[],[15,53,83]).
/*
[_A,_B,_C]:
  [=>(_A,_B,_D),=>(_A,_C,_E),<(_B,_D,_F),
  <(_C,_E,_G),<(_F,_E,_H),<(_G,_D,_I)] = 
[_A => 15,_H => 53,_I => 83].
*/

% Fredkin gate variant - paper Dueck ??
% A,ite0(A,B,C),ite1(A,B,C) 3x3
m16:-syn(3,8,['=>','<'],[],[15,29,71]).
/*
?- m16.
[_A,_B,_C]:
  [=>(_B,_A,_D),
   =>(_B,_C,_E),
    <(_A,_D,_F),
    <(_C,_E,_G),
    <(_F,_E,_H),
    <(_G,_D,_I)] 
= [_A => 15,_H => 29,_I => 71].
*/


% mutual synthesis sym in terms of asym and viceversa

m17:- syn(2,6,[<,=>],[],[8,14]).
%[_A,_B]:[<(_A,_A,_C),=>(_A,_C,_D),<(_B,_D,_E),=>(_B,_D,_F)] = [o(_E,8),o(_F,14)]

m18:-syn(2,6,[nand,nor],[],[4,13]).
% [_A,_B]:[nand(_B,_B,_C),nand(_C,_A,_D),nor(_C,_A,_E)] = [o(_E,4),o(_D,13)].

m19:-syn(2,6,[nand,nor],[0,1],[4,13]).
% [_A,_B]:[nor(0,_B,_C),nand(_C,_A,_D),nor(_C,_A,_E)] = [o(_E,4),o(_D,13)].


m20:- syn(2,6,[<,=>],[0,1],[8,14]).
% [_A,_B]:[=>(_A,0,_C),<(_B,_C,_D),=>(_B,_C,_E)] = [o(_D,8),o(_E,14)].


% hard examples - long ints needed !!!
h1:-syn(['=>','<'],[],5:1).
h2:-syn(['nand','nor'],[],5:1).

h3:-syn(['=>','<'],[],3:105). %A xor B xor C
% 105=((A=>((B =>C)=>(B<C)))=>(A<((B=>C)=>(B<C)))).
h4:-syn(['=>','<'],[],4:5335).

h5:-syn(['nand','nor'],[],3:105).
h6:-syn(['nand','nor'],[],4:5335).

% sample outputs
/*
?- ulib(3,3,L),println(L),fail.
[nand]+[]
[<]+[1]
[>]+[1]
[=>]+[0]
[<=]+[0]
[nor,<]+[]
[nor,>]+[]
[nor,=>]+[]
[nor,<=]+[]
[nor,*]+[]
[nor,+]+[]
[nor,=]+[]
[nor,^]+[]
[<,=>]+[]
[<,<=]+[]
[<,=]+[]
[<,nhead]+[]
[<,ntail]+[]
[>,=>]+[]
[>,<=]+[]
[>,=]+[]
[>,nhead]+[]
[>,ntail]+[]
[=>,^]+[]
[=>,nhead]+[]
[=>,ntail]+[]
[=>,zero]+[]
[<=,^]+[]
[<=,nhead]+[]
[<=,ntail]+[]
[<=,zero]+[]
[*,=]+[0]
[*,^]+[1]
[*,nhead]+[]
[*,ntail]+[]
[+,=]+[0]
[+,^]+[1]
[+,nhead]+[]
[+,ntail]+[]
[<,*,one]+[]
[>,*,one]+[]
[*,=,^]+[]
[*,=,zero]+[]
[*,^,one]+[]
[+,=,^]+[]
no

?- ulib(3,4,L),println(L),fail.
[nand]+[]
[nor]+[]
[<]+[1]
[>]+[1]
[=>]+[0]
[<=]+[0]
[<,=>]+[]
[<,<=]+[]
[<,=]+[]
[<,nhead]+[]
[<,ntail]+[]
[<,one]+[]
[>,=>]+[]
[>,<=]+[]
[>,=]+[]
[>,nhead]+[]
[>,ntail]+[]
[>,one]+[]
[=>,^]+[]
[=>,nhead]+[]
[=>,ntail]+[]
[=>,zero]+[]
[<=,^]+[]
[<=,nhead]+[]
[<=,ntail]+[]
[<=,zero]+[]
[*,=]+[0]
[*,^]+[1]
[*,nhead]+[]
[*,ntail]+[]
[+,=]+[0]
[+,^]+[1]
[+,nhead]+[]
[+,ntail]+[]
...
*/

% end


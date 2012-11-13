% computes value O of bitstring-int truth table built with any of the 16 2-arg ops
% when applied to two bitstring-int encoded arguments representing varioables or
% intermediate results of expressions. The output O is an integer representing a 
% truth table as a bit vector of length 2^NbOfBits

eval_bitstring_int_formula(T,NbOfBits, O):-  
  exp2(NbOfBits,TTBits),
  eval_bitstring_int(T,TTBits,O).

% 
eval_bitstring_int(T,NbOfBits, R):-
  compound(T),
  !,
  functor(T,Op,2),
  operator_to_bitstring_int_truth_table(Op,F),arg(1,T,X),arg(2,T,Y),
  eval_bitstring_int(X,NbOfBits,A),
  eval_bitstring_int(Y,NbOfBits,B),
  eval_operator_on_bitstring_ints(NbOfBits,F,A,B,R).
eval_bitstring_int(X,_NbOfBits,X).

eval_operator_on_bitstring_ints(NbOfBits,F,A,B,R):-
  int2lbits(4,F,Fs),
  int2lbits(NbOfBits,A,As),
  int2lbits(NbOfBits,B,Bs),
  combine_bitstring_int_truth_tables(As,Bs,NbOfBits,Fs,Rs),
  lbits2int(Rs,R).

combine_bitstring_int_truth_tables([],[],_NbOfBits,_Fs,[]).
combine_bitstring_int_truth_tables([A|As],[B|Bs],NbOfBits,Fs,[R|Rs]):-
  apply_bitstring_int_operator_to_bits(Fs,A,B,R),
  combine_bitstring_int_truth_tables(As,Bs,NbOfBits,Fs,Rs).

apply_bitstring_int_operator_to_bits(Fs,A,B,R):-
  C is 1+(2*A)+B,
  nth_member(R0,Fs,C),
  !,
  R0=R.

% var in 0..k to truth table seen as int - faster
% uses trick from see Knuth 2006 - Boolean Evaluation
var_to_bitstring(NbOfBits,K,Xk):-
  all_ones(NbOfBits,Mask),
  NK is NbOfBits-(K+1),
  D is (1<<(1<<NK))+1,
  Xk is Mask//D.
  
% max bitstring int of the form 11...1 build with NbOfBits bits 
all_ones(NbOfBits,Mask):-Mask is (1<<(1<<NbOfBits))-1. 

vars_to_bitstrings(Us):-vars_to_bitstrings(Us,_NbOfBits).

% bind N free vars to their N-bit bitstring_int representation
vars_to_bitstrings(Us,NbOfBits):-
  length(Us,NbOfBits),
  vars_to_bitstrings(Us,NbOfBits,0,NbOfBits).

vars_to_bitstrings([],_,N,N).
vars_to_bitstrings([X|Xs],NbOfBits,N1,N2):-
  N is N1+1,
  var_to_bitstring(NbOfBits,N1,X),
  vars_to_bitstrings(Xs,NbOfBits,N,N2).

% bitstring int operations

bitand(X1,X2,X3):-X3 is '/\'(X1,X2).
bitor(X1,X2,X3):-X3 is '\/'(X1,X2).
bitxor(X1,X2,X3):-X3 is '#'(X1,X2).
bitless(X1,X2,X3):-X3 is '#'(X1,'\/'(X1,X2)).
bitgt(X1,X2,X3):-X3 is '#'(X1,'/\'(X1,X2)).
bitnot(NbOfBits,X1,X3):-all_ones(NbOfBits,M),X3 is '#'(X1,M).
biteq(NbOfBits,X1,X2,X3):-all_ones(NbOfBits,M),X3 is '#'(M,'#'(X1,X2)).
bitimpl(NbOfBits,X1,X2,X3):-bitnot(NbOfBits,X1,NX1),bitor(NX1,X2,X3).
bitnand(NbOfBits,X1,X2,X3):-bitand(X1,X2,NX3),bitnot(NbOfBits,NX3,X3).
bitnor(NbOfBits,X1,X2,X3):-bitor(X1,X2,NX3),bitnot(NbOfBits,NX3,X3).
bitite(NbOfBits,C,T,E,R):-
  all_ones(NbOfBits,M),
  NC is '#'(C,M),
  R is '\/'('/\'(C,T),'/\'(NC,E)).

operator_to_bitstring_int_truth_table((zero),0).
operator_to_bitstring_int_truth_table((*),1).
operator_to_bitstring_int_truth_table((>),2).
operator_to_bitstring_int_truth_table((head),3).
operator_to_bitstring_int_truth_table((<),4).
operator_to_bitstring_int_truth_table((tail),5).
operator_to_bitstring_int_truth_table((^),6).
operator_to_bitstring_int_truth_table((+),7).
operator_to_bitstring_int_truth_table((nor),8).
operator_to_bitstring_int_truth_table((=),9).
operator_to_bitstring_int_truth_table((ntail),10).
operator_to_bitstring_int_truth_table((<=),11).
operator_to_bitstring_int_truth_table((nhead),12).
operator_to_bitstring_int_truth_table((=>),13).
operator_to_bitstring_int_truth_table((nand),14).
operator_to_bitstring_int_truth_table((one),15).

% gets right bit I of T, first bit is bit 0
getrbit(T,I,R):-R is '/\'(T>>I,1).

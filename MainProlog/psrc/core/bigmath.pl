% reflection interface primitives

bigint_constructor(Args,R):-
  new_java_class('java.math.BigInteger',C),
  new_java_object(C,Args,R).  

random_bigint(Bits,BigR):-
 call_java_class_method('prolog.logic.Tools',getRandom,R),
 bigint_constructor(args(Bits,R),BigR).

% int2bigint(N,E):-number_codes(N,Cs),string2bigint(Cs,E).

int2bigint(N,E):-call_java_class_method('java.math.BigInteger',valueOf(N),E).

string2bigint(Is,BI):-val(radix,Radix),!,string2bigint(Radix,Is,BI).
string2bigint(Is,BI):-bigint_constructor(args('$STRING_CS2S'(Is)),BI).

bits_to_bigint(Is,BI):-string2bigint(2,Is,BI).

string2bigint(Radix,Is,BI):-bigint_constructor(args('$STRING_CS2S'(Is),Radix),BI).

get_radix(Radix):-val(radix,R),!,Radix=R.
get_radix(10).

get_intlen(Len):-val(intlen,B),!,Len=B. % negaitve size means left padding with 0
get_intlen(0).

bigint2string(I,Is):-integer(I),!,number_codes(I,Is).
bigint2string(BI,Is):-
  get_radix(Radix),
  bigint2string(Radix,BI,Is).

bigint2string(Radix,BI,Is):-
  get_intlen(Len),
  object_to_codes(Len,Radix,BI,Is).
  
% prints a list of char codes as a unit
print_codes(Cs):-map(put,Cs).

% prints a BigInteger object directly
bigint_print(B):-bigint2string(B,Cs),print_codes(Cs).

bigbits_print(B):-bigint2string(2,B,Cs),print_codes(Cs).

% note - works with forms as 123,"123"

is_bigint(X,A):-var(X),!,is_bigint0(A,R),X=R.
is_bigint(A,X):-var(X),!,is_bigint0(A,R),X=R.
is_bigint(A,B):-is_bigint0(A,R1),is_bigint0(B,R2),R1=R2.

is_bigint0(ExprCodes,RCodes):-
  build_big_expr(ExprCodes,BigExpr),
  eval_big_expr(BigExpr,BigR,Os,[]),
  bigint2string(BigR,RCodes),
  delete_java_objects(Os).

to_bigint(N,B):-integer(N),!,int2bigint(N,B).
to_bigint([C|Cs],B):-string2bigint([C|Cs],B).
to_bigint(N,B):-atom(N),!,atom_codes(N,Cs),string2bigint(Cs,B).

build_big_expr(N,E):-integer(N),!,int2bigint(N,E).
build_big_expr([C|Cs],E):-!,string2bigint([C|Cs],E).
build_big_expr(N,B):-atom(N),!,atom_codes(N,Cs),string2bigint(Cs,B).
build_big_expr('$object'(BI),B):-!,B='$object'(BI). % already a BigInteger object
build_big_expr(radix(N,[C|Cs]),E):-!,string2bigint(N,[C|Cs],E).
build_big_expr(random(Bits),E):-!,random_bigint(Bits,E).
build_big_expr(T,E):-T=..[Op|Args],map(build_big_expr,Args,BigArgs),E=..[Op|BigArgs].

big_op_code('+',add).
big_op_code('-',subtract).
big_op_code('*',multiply).
big_op_code('/',divide).

big_op_code('/\',and).
big_op_code('\/',or).
big_op_code('#',xor).
big_op_code('~',(not)).
big_op_code('\',andNot).
big_op_code('to_double',doubleValue).
big_op_code('to_int',intValue).
big_op_code('next_prime',nextProbablePrime).
/* % implicit
big_op_code('max',max).
big_op_code('min',min).
big_op_code('mod',mod).
big_op_code('gcd',gcd).
....
*/
big_op_int_code('<<',leftShift). %int
big_op_int_code('>>',rightShift). %int
big_op_int_code('setBit',setBit). %int
big_op_int_code('pow',pow). %int

eval_big_expr('$object'(B),R)-->!,{R='$object'(B)},[R].
eval_big_expr(E,R)-->{E=..[Op|Args]},
   map_eval_big_expr(Args,Bs),
  {bigint_op(Op,Bs,R)}.

map_eval_big_expr([],[])-->[].
map_eval_big_expr([A|As],[B|Bs])-->
  eval_big_expr(A,B),
  map_eval_big_expr(As,Bs).

bigint_op(testBit,[B,BI],R):-!,
  big2int(BI,I),
  invoke_java_method(B,testBit(I),BR),
  bool2int(BR,IR),
  int2bigint(IR,R).
bigint_op(IntOp,[B,BI],R):-big_op_int_code(IntOp,Op),!,
  big2int(BI,I),
  Meth=..[Op,I],
  invoke_java_method(B,Meth,R).
bigint_op(Op,[B|Bs],R):-
  op2meth(Op,F),
  Meth=..[F|Bs],
  invoke_java_method(B,Meth,R).	

op2meth(Op,BigOp):-big_op_code(Op,BigOp),!.
op2meth(Op,Op).

bigint_rel(A<B):-is_bigint(compareTo(A,B),R),R<0.
bigint_rel(A>B):-is_bigint(compareTo(A,B),R),R>0.
bigint_rel(A=<B):-is_bigint(compareTo(A,B),R),R=<0.
bigint_rel(A>=B):-is_bigint(compareTo(A,B),R),R>=0.
bigint_rel(A=:=B):-is_bigint(compareTo(A,B),R),R=:=0.
bigint_rel(A=\=B):-is_bigint(compareTo(A,B),R),R=\=0.

%bigint2int
big2int(B,I):-invoke_java_method(B,intValue,I).

% BigMath interface

call_bigmath(Method,R):-call_java_class_method('prolog.core.BigMath',Method,R).

bigint2tuple(IntArity,BigInt,Tuple):-call_bigmath(bigint2tuple(IntArity,BigInt),Tuple).

bigint2tuple(BigInt,Tuple):-bigint2tuple(2,BigInt,Tuple).

bigint2igraph(BigInt,Cat):-call_bigmath(bigint2igraph(BigInt),Cat).

igraph2bigint(Cat,BigInt):-call_bigmath(igraph2bigint(Cat),BigInt).

tuple2bigint(Tuple,BigInt):-call_bigmath(tuple2bigint(Tuple),BigInt).

bigint2exps(BigInt,Tuple):-call_bigmath(bigint2exps(BigInt),Tuple).

exps2bigint(Tuple,BigInt):-call_bigmath(exps2bigint(Tuple),BigInt).

show_tuple(Tuple):-call_bigmath(tupleToString(Tuple),S),println(S).

tuple2strings(Tuple,Css):-array_to_list(Tuple,Ts),map(bigint2string,Ts,Css).

bigint2cat(Arity,N,C):-bigint2cat(2,Arity,N,C).

bigint2cat(MaxUrInt,Arity,N,C):-call_bigmath(bigint2cat(MaxUrInt,Arity,N),C).

max_big_of(Cat,B):-call_bigmath(maxBigOf(Cat),B).

bigint2gray(B,G):-call_bigmath(bigint2gray(B),G).

gray2bigint(G,Bits,B):-call_bigmath(gray2bigint(G,Bits),B).

bigint2cat(N,C):-bigint2cat(2,N,C).

ints2cat(Arity,N,C):-call_bigmath(ints2cat(Arity,N),C).

lvar2bigint(NBits,NVar,B):-call_bigmath(lvar2bigint(NBits,NVar),B).

bigones(NBits,BigOnes):-call_bigmath(bigones(NBits),BigOnes).

bignot(NBits,B,NotB):-call_bigmath(bignot(NBits,B),NotB).

% test

bigfact(N,R):- N is_bigint 0,!,
  R is_bigint 1.
bigfact(N,R):- 
  bigint_rel(N>0),
  N1 is_bigint N-1,
  bigfact(N1,R1),
  R is_bigint N*R1.

bigtest:-
  string2bigint(2,"111",A),
  string2bigint("-4",X),
  bigint_op(abs,[X],B),
  bigint_op(add,[A,B],C),
  let(radix,2),write('base 2:'),bigint_print(A),nl,rm(radix),
  write('abs:'),bigint_print(B),nl,
  write('add:'),bigint_print(C),nl,
  delete_java_objects([A,B,C]),
  bigfact(100,R),
  write('bigfact:'),print_codes(R),nl,
  BR is_bigint random(128), % 128=bits
  write('bigrandom:'),print_codes(BR),nl,
  string2bigint(BR,BJ),bigint2tuple(BJ,Us),tuple2bigint(Us,BJ1),
  write('equal_to_bigrandom:'),bigint_print(BJ1),nl,
  string2bigint(R,BI),
  bigint2tuple(16,BI,Ts),
  show_tuple(Ts),
  bigint2exps(BI,Es),
  show_tuple(Es),
  tuple2strings(Ts,Ss),
  foreach(member(S,Ss),(print_codes(S),put(32))),nl.  

graytest:-
  let(radix,10),let(intlen,0),
  for(I,0,15),
  write(I),write(':'),nl,
  int2bigint(I,B),
  bigint2gray(B,G),
  gray2bigint(G,4,R),
  let(radix,2),let(intlen,-4),
  write(b),bigint_print(B),nl,
  write(g),bigint_print(G),nl,
  write(r),bigint_print(R),nl,
  nl,
  let(radix,10),let(intlen,0),
  fail
; true.
    
jbigtest:-call_bigmath(test,_).


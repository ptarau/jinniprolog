% '/'(A,B,C):-name(One,"1.1"),'*'(A,One,FA), '*'(B,One,FB),'//'(FA,FB,C).

'/'(A,B,C):-integer(A),integer(B),!,call_java_class_method('prolog.kernel.Machine',divide(A,B),C).
'/'(A,B,C):-'//'(A,B,C).

compute_call(Op,A,R):-
  new_java_class('java.lang.Math',Math),
  invoke_java_method(Math,Op,args(A),R).
compute(Op,A,R):-compute_call(Op,A,R).

compute(Op,A,B,R):-compute0(Op,A,B,C),!,R=C.
compute(Op,A,B,R):-compute_call(Op,A,B,R).

compute0('+',A,B,C):- '+'(A,B,C).
compute0('*',A,B,C):- '*'(A,B,C).
compute0('-',A,B,C):- '-'(A,B,C).
compute0('//',A,B,C):- '//'(A,B,C).
compute0('/',A,B,C):- '/'(A,B,C).
compute0('?',A,B,R):- compare0(A,B,C),
  case(C,['<' : (R= -1), ('=') : (R=0), '>' : (R=1)]).

compute_call(Op,A,B,R):-
  new_java_class('java.lang.Math',Math),
  invoke_java_method(Math,Op,args(A,B),R).
    
pow(A,X,R):-compute_call('pow',A,X,R1),'+'(R1,0,R).
exp(A,B) :-compute_call('exp',A,B).
log(X,R):-compute_call('log',X,R).
log(A,X,R):-log(X,LX),log(A,LA),'//'(LX,LA,R).
sqrt(X,R):-compute_call('sqrt',X,R1),'+'(R1,0,R).
abs(X,R):-compute_call('abs',X,R).
ceil(X,R):-compute_call('ceil',X,R1),'+'(R1,0,R).
sin(X,R):-compute_call('sin',X,R).
cos(X,R):-compute_call('cos',X,R).
tan(X,R):-compute_call('tan',X,R).
asin(X,R):-compute_call('asin',X,R).
acos(X,R):-compute_call('acos',X,R).
atan(X,R):-compute_call('atan',X,R).
floor(X,R):-compute_call('floor',X,R1),'+'(R1,0,R).
round(X,R):- '/'(1,2,D),'+'(X,D,X1),floor(X1,R).
% for compatibilty with some Prologs
integer(Double,Int):-floor(Double,Int).

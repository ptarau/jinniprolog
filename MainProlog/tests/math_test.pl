compute(Op,A,B,R):-
  new_java_class('java.lang.Math',Math),
  invoke_java_method(Math,Op,args(A,B),R).

pow(A,B,R):-compute(pow,A,B,R).

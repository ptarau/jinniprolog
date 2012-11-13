test:-
   new_java_class('MyMain',Class),
   Term=f(X,g(3.14,hello,X,99),h(Y,Y)),
   invoke_java_method(Class,workOnPrologData(Term),Result),
   println(Result).

likes('Joe',beer).
likes('Mary',wine).

:-println('PLEASE TYPE test TO RUN THE OTHER EXAMPLES').

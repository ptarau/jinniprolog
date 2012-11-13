% tak benchmark

tak(X,Y,Z,A) :- X =< Y, !, Z = A.
tak(X,Y,Z,A) :-
        X1 is X - 1,    
        Y1 is Y - 1,    
        Z1 is Z - 1,    
        tak(X1,Y,Z,A1), 
        tak(Y1,Z,X,A2), 
        tak(Z1,X,Y,A3), 
        tak(A1,A2,A3,A).

go:-    statistics(runtime,_), 
        tak(24,16,8,X),
        statistics(runtime,[_,T]),statistics, 
        write([time=T,tak=X]), nl.

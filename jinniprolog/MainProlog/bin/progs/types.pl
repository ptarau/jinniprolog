known_type('+'(num,num), num).
known_type('*'(num,num), num).
known_type('-'(num,num), num).
known_type('/'(num,num), num).

known_type(X,num):-number(X).
known_type(X,string):-atom(X),X\==[].

% inc x = x+1

fundef(inc(X),'+'(X,num)).

% inc_all [] = []
% inc_all (x:xs) = (inc x):(inc_all xs)

/* ???
fundef(inc_all(list(X)),list(X)).
fundef(inc_all([X|Xs]),[inc(X)|inc_all(Xs)]).
*/

% ?-infer_type(inc(_),Type).


infer_type(Known,Type):-known_type(Known,T),!,Type=T.
infer_type(Head,Type):-
   fundef(Head,Body),
   infer_type(Body,Type).
infer_type([],list(_)).
infer_type([X|Xs],list(T)):-
   infer_type(X,T),
   infer_type(Xs,list(T)).


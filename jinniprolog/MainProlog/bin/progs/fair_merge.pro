%:-[ja_prime].

imerge(I,J,R):-
  new_engine(ignore,fair_merge(I,J,[],[]),R).

fair_merge(I,J,Xs,Ys):-
  lazy_next(I,X,Xs,NewXs),return(X),
  lazy_next(J,Y,Ys,NewYs),return(Y),
  fair_merge(I,J,NewXs,NewYs).
  
lazy_next(_,X,Xs,Xs):-rmember(X,Xs).
lazy_next(I,X,Xs,[X|Xs]):-get(I,X).
  
% reverse member
%rmember(X,[_|Ys]):-rmember(X,Ys).
%rmember(X,[X|_]).

first_n_primes(N,P):-prime_engine(E),for(_,1,N),get(E,the(P)).

prime(P):-prime_engine(E),element_of(E,P).

prime_engine(E):-new_engine(_,test_prime(1,_),E).
   
test_prime(N,Ps):-N1 is N+1,pmember(N1,Ps),test_prime(N1,Ps).

pmember(N,[P|_]):-var(P),!,P=N,return(P).
pmember(N,[P|_]):-N mod P =:=0,!.
pmember(N,[_|Ps]):-pmember(N,Ps).


/*
pmember(P,[N|Ns]):-nonvar(N),P mod N =\=0,!,pmember(P,Ns).
pmember(P,[P|_]):-!,return(P).
pmember(_,_).
*/

% end

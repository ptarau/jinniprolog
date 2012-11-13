first_n_primes(N,P):-prime_engine(E),for(_,1,N),get(E,the(P)).

% stream of primes, on backtracking
prime(P):-prime_engine(E),element_of(E,P).

prime_engine(E):-new_engine(_,test_prime(1,_),E).
   
test_prime(N,Ps):-N1 is N+1,pmember(N1,Ps),test_prime(N1,Ps).

pmember(N,[P|_]):-var(P),!,P=N,return(P).
pmember(N,[P|_]):-N mod P =:=0,!.
pmember(N,[_|Ps]):-pmember(N,Ps).

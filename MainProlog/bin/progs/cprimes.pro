first_n_primes(N,P):-prime_engine(E),for(_,1,N),get(E,the(P)).

% stream of primes, on backtracking
prime(P):-prime_engine(E),element_of(E,P).

prime_engine(E):-new_engine(_,new_prime(1),E).

% returns a new prime 
new_prime(N):-
  N1 is N+1,
  test_prime(N1),
  new_prime(N1).

% checks if N is prime
test_prime(N):-
  M is integer(sqrt(N)),
  for(D,2,M),
    N mod D =:=0,
  !.
test_prime(N):-
  return(N).

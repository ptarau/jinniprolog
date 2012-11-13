% backtracking integer partition iterator
 
integer_partition_of(N,Ps):-
  nats(N,Is),
  split_to_sum(N,Is,Ps).

split_to_sum(0,_,[]).
split_to_sum(N,[K|Ks],R):-
  N>0,
  sum_choice(N,K,Ks,R).

sum_choice(N,K,Ks,[K|R]):-
  NK is N-K,
  split_to_sum(NK,[K|Ks],R).
sum_choice(N,_,Ks,R):-
  split_to_sum(N,Ks,R).

nats(1,[1]).
nats(N,[N|Ns]):-
  N>1,
  N1 is N-1,
  nats(N1,Ns).

count_partitions(N,R):-
  new_engine(1,integer_partition_of(N,_),Engine),
  efoldl(Engine,+,0,R).
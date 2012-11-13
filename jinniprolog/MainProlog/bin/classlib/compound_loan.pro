:-[loan].
  
compound_loan(Principal,Rate,Duration,Periods):-
  loan(Principal,Rate,Duration),
  periods<=Periods.
  
amount(A):-
  principal=>P,
  rate=>R,
  duration=>T,
  periods=>N,
  A is P*pow(1+(R/N),(N*T)).

payment(P):-
  amount(A),
  periods=>N,
  duration=>T,
  P is A/(N*T).
  
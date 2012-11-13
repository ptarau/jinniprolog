:-[compound_loan].
  
mortgage(Principal,Rate,Duration,Periods,Points):-
  compound_loan(Principal,Rate,Duration,Periods),
  points<=Points.
  
payment(M):-
  periods=>N,
  duration=>T,
  rate=>I,
  J is I/N,
  NT is N*T,
  principal=>P,
  M is P*J/(1-pow(1+J, -(NT))).
  
amount(A):-
  payment(M),
  periods=>N,
  duration=>T,
  A0 is M*N*T,
  points=>X,
  principal=>P,
  Extra is P*(X/100.0),
  A is A0+Extra.

cost(C):-
  principal=>P,
  amount(A),
  C is A-P.
  
/*
?- new(mortgage(100000,0.08,30,12,0),L),L:payment(X).
L = '$instance'(mortgage@1084,3) X = 733.7645738795254 ;

*/

  
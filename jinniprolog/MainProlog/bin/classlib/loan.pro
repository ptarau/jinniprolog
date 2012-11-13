loan(Principal,Rate,Duration):-
  principal<=Principal,
  rate<=Rate,
  duration<=Duration.
  
interest(I):-
  principal=>P,
  amount(A),
  I is A-P.
  
amount(A):-
  principal=>P,
  rate=>R,
  duration=>T,
  A is P*(1+R*T).

fredkin_gate(C,P,Q,C,NewP,NewQ):-
  l_not(C,NC),
  l_and(C,P,CP),
  l_and(C,Q,CQ),
  l_and(NC,P,NCP),
  l_and(NC,Q,NCQ),
  l_or(CQ,NCP,NewP),
  l_or(CP,NCQ,NewQ).

l_not(0,1).
l_not(1,0).

l_and(0,0,0).
l_and(0,1,0).
l_and(1,0,0).
l_and(1,1,1).

l_or(0,0,0).
l_or(0,1,1).
l_or(1,0,1).
l_or(1,1,1).



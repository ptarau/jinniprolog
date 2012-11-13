% counting trees and leaf-dags

% 1, 2, 5, 14, 42, 132, 429, 1430, 4862, 16796
catalan(1,1).
catalan(N1,R1):-
  N1>1,
  N is N1-1,
  catalan(N,R),
  R1 is R*(4*N+2)/(N+2).
  
leafdags(MGates,NPIs,R):-
  catalan(MGates,C),
  NbLeaves is MGates+1,
  F is pow(NPIs,NbLeaves),
  R is C*F.

ngraph_to(Max,R):-
  for(N,0,Max),
  count_answers(graph(N,_),R).

ngraphs(Max,Rs):-findall(R,ngraph_to(Max,R),Rs).

best_of(Answer,Comparator,Generator):-
  new_engine(Answer,Generator,E),
  efoldl(E,compare_answers(Comparator),no,Best),
  Answer=Best.

compare_answers(_Comparator,no,A,Best):-Best=A.
compare_answers(Comparator,A1,A2,Best):-
  A1\==no,
  if(call(Comparator,A1,A2),Best=A1,Best=A2).

test_best(X):-
  best_of(X,>,member(X,[2,1,4,3])).

% ?-test_best(X).
% X=4.

% end

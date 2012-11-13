apply_clause_op(ClauseOP,ClauseS,SoFar):-
  good_element_of(ClauseS,C0),
  to_clause(C0,C),
  C=(H:-_),functor(H,F,N),
  \+pred_already_seen(F/N,SoFar),
  call(ClauseOP,C),
  fail.
apply_clause_op(_,_,_).

not_seen_file(I,Is,SoFar):-
  member(I,Is),
  \+file_already_included(I,SoFar).

not_seen_pred(P,Ps,SoFar):-
  member(P,Ps),
  \+pred_already_seen(P,SoFar).


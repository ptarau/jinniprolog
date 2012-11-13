% does not exhibit the problem - which might have
% got fixed as a result of other fixes

bug:-
  bug([(X:p(X))],[aaa],[]).

bug([])-->[].
bug([X|Xs])-->
  match_intension(X),
  bug(Xs).

match_intension(XP)-->
  [X],
  {println(here(XP)),
   XP=(X:P),
   println(X+P)
  }.

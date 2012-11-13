% only works with consulted code that exposes names

help(Name):-apropos(Name).

apropos(Name):-
  findall(FN,apropos(Name,FN),FNs),
  sort(FNs,Xs),
  member(X,Xs),
    write(X),nl,
  fail.
apropos(_).

apropos(Name,F/N-[Prop|Info]):-
  name(Name,UnKnown),
  predicate_property(H,Prop),functor(H,F,N),
  name(F,Known),
  near_match(Known,UnKnown),
  Info=[].

near_match(Known,Known):-!.
near_match(Known,UnKnown):-append3(_,UnKnown,_,Known),!.
near_match(Known,UnKnown):-append3(A,B,C,Known),append3(A,[_|B],C,UnKnown),!.
%near_match(Known,UnKnown):-near_match(UnKnown,Known).

append3(Xs,Ys,Zs,Us):-
  append(Xs,YsZs,Us),
  append(Ys,Zs,YsZs).

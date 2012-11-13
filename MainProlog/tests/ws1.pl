write_ws(Ws):-
  words2nat(Ws,Wss),member(Ns,Wss),member(N,Ns),name(N,Cs),member(C,Cs),put(C),fail
; true.

words2nat(Ws,Wss):-words2nat(Wss,Ws,[]).

words2nat([])-->[].
words2nat([[W,C,' ']|Wss])-->[W,C],{left_collider(C)},!,words2nat(Wss).
words2nat([[L,C]|Wss])-->[L,C],{collider(C)},!,words2nat(Wss).
words2nat([[Q,W,Q,' ']|Wss])-->[Q],{Q=('"')},[W],[Q],!,words2nat(Wss).
words2nat([[W,' ']|Wss])-->[W],words2nat(Wss).

left_collider(W):-member(W,[(','),(':'),(';'),('.'),('!'),('?')]).
collider(W):-member(W,[(''''),(-)]).


go:-
  codes_words("Look: the more-than-blind-man's dog, the ""devil""'s bride; he's ""god's fool""!",Ws),
  println(Ws),
  write_ws(Ws),
  nl.



  
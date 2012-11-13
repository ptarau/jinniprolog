/*
write_words(Ws):-nonvar(Ws),!,write_ws(Ws).
write_words(Ws):-errmes(list_expected,found(Ws)).

write_ws(Ws):-words2nat(Ws,Ns),member(N,Ns),name(N,Cs),member(C,Cs),put(C),fail.
write_ws(_).

words2nat(Ws,Ns):-words2nat(Wss,Ws,[]),println(Wss),appendN(Wss,Vs),once(append(Us,[S],Vs)),(' '=S->Ns=Us;Ns=Vs).

words2nat([])-->[].
words2nat([[W,C,' ']|Wss])-->[W,C],{left_collider(C)},!,words2nat(Wss).
words2nat([[L,C]|Wss])-->[L,C],{collider(C)},!,words2nat(Wss).
%words2nat([[Q,W,Q,' ']|Wss])-->[Q],{Q=('"')},[W],[Q],!,words2nat(Wss).
words2nat([[Q|Vs],[Q]|Wss])-->[Q],{Q=('"')},match_before(Q,Ws),!,{words2nat(Ws,Vs)},words2nat(Wss).
words2nat([[W,' ']|Wss])-->[W],words2nat(Wss).

left_collider(W):-member(W,[(','),(':'),(';'),('.'),('!'),('?')]).

collider(W):-member(W,[(''''),(-)]).

match_to(Stop,Cs)-->match_to([Stop],Cs,_).
match_to(Stops,[],Stop)-->look_ahead(Stop),{member(Stop,Stops)},!.
match_to(Stops,[C|Cs],Stop)-->[C],match_to(Stops,Cs,Stop).

insert_spaces([],[]).
insert_spaces([W],[W]):- !.
insert_spaces([W1,W2|Ws],[W1,' '|Vs]):-insert_spaces([W2|Ws],Vs).

look_ahead(Next,[Next|Cs],[Next|Cs]).
*/



go:-
  codes_words(
    "Look: the more-than-blind-man's dog, the ""devil""'s bride; he's ""god's damn poor fool"",he!",Ws),
  println(Ws),
  codes_words(NewCs,Ws),
  forall(member(C,NewCs),put(C)),
  nl.



  
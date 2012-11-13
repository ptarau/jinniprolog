abug:-
  abug(20).
  
abug(Max):-
  for(I,1,Max),
  namecat(s,I,P),
  new(
    gui_agent(P),
    _
  ),
  fail
; true.


sbug:-
  sbug(20).
  
sbug(Max):-
  for(I,1,Max),
  namecat(s,I,P),
  new(
    gui_agent(P,
      (for(J,1,Max),println(I=>J),fail;true)
    ),
    _
  ),
  fail
; true.

cbug:-
  Max=100,
  for(_I,1,Max),
  bg(console),
  fail
; true.

bbug:-
  Max=10,
  ( 
    for(I,1,Max),
    create(I),
    fail
  ; true
  ),
  % sleep(5),
  (
    for(I,1,Max),
    test_hi(I),
    fail
  ; true
  ),
  (
    for(I,1,Max),
      for(J,1,Max),
        msg(I,J),
        msg(J,I),
    fail
  ; true
  ).
  
create(I):-
  namecat(a,'_',I,P),
  new(gui_agent(P),_).

  
test_hi(I):-
  namecat(a,'_',I,P),
  rli_hi(P).

  
msg(J,I):-
  namecat(a,'_',I,P),
  rli_call(P,
    println(
      from(J,to(I))
    )
  ).

      

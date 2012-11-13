% :-initialization(println(initializing(class,shape))).

shape:-
  centerX<=0,
  centerY<=0.

show:-
  centerX=>X,
  centerY=>Y,
  println('I am a shape at'(X,Y)).

move_to(X,Y):-
  centerX<=X,
  centerY<=Y.

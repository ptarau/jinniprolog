:-[shape].

:-initialization(println(initializing(class,circle))).
:-initialization(assert(good(shape))).

circle:-
  shape,
  radius<=1.

circle(X,Y,R):-
  centerX<=X,
  centerY<=Y,
  radius<=R.

show:-
  centerX=>X,
  centerY=>Y,
  % new(shape,S),S:move_to(X,Y),S:show,
  radius=>R,
  println('I am a circle at'(X,Y,R)).

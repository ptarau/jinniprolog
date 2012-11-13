:-[circle].

fixed_circle(X,Y,R):-circle(X,Y,R).

move_to(X,Y):-errmes(cannot_move,to(X,Y)).

% Visual N queens program

go:-go(8).

go(N):-go(N,200).

go(N,Size):-
  init_queens(N,DSize,Frame,QPanel),
  set_direction(Frame,'South'),
  new_panel(Frame,grid(1,1),DPanel),
  new_label(DPanel,'Copyright (C) Paul Tarau 2004',_),
  resize(QPanel,Size,Size),
  resize(DPanel,Size,DSize),
  show(Frame),
  (queens(N,Qs),
     draw_queens(N,Qs,QPanel),
     show(QPanel),
     remove_all(DPanel),
     dialog_in(DPanel,'More?',Answer),
     Answer=no
  ;  true
  ),
  !,
  remove_all(Frame),
  destroy(Frame).

% visualisation

init_queens(N,DSize,Frame,Panel):-
  new_frame('N-Queens',border,0,Frame),
  DSize=72,
  set_direction(Frame,'Center'),
  new_panel(Frame,grid(N,N),Panel),
  new_color(0,0,1,Blue),
  set_bg(Panel,Blue).
  
draw_queens(N,Qs,Frame):-
  '=..'(Queens,[queens|Qs]),
  remove_all(Frame),
  new_color(1,0,0,Red),
  new_color(0,1,0,Green),
  for(I,1,N),for(J,1,N),
    if(arg(I,Queens,J),
      put_image(Frame,'Q',Red),
      put_image(Frame,' ',Green)
    ),
  fail.
draw_queens(_,_,_).

put_image(Frame,Text,Color):-
   new_label(Frame,Text,P),
   set_bg(P,Color).

% queens program

queens(N,Ps):-
  gen_places(N,Ps),
  gen_queens(N,Qs),
  place_queens(Qs,Ps,_,_).

place_queens([],_,_,_).
place_queens([I|Is],Cs,Us,[_|Ds]):-
        place_queens(Is,Cs,[_|Us],Ds),
        place_queen(I,Cs,Us,Ds).

place_queen(I,[I|_],[I|_],[I|_]).
place_queen(I,[_|Cs],[_|Us],[_|Ds]):-
        place_queen(I,Cs,Us,Ds).

gen_places(Max,Ps):-
  findall(_,for(_,1,Max),Ps).

gen_queens(Max,Qs):-
  findall(Q,for(Q,1,Max),Qs).

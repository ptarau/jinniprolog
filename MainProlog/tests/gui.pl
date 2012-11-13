    
/* examples */

gtest:-
  new_frame(foo,grid(2,2),F),
  resize(F,300,300),
  new_panel(F,flow,P),
  new_label(P,hello,L),
  set_label(L,bye),
  new_panel(F,flow,TP),
  new_text(TP,'example',0,0,_T),
  new_panel(F,flow,TB),
  new_button(TB,hello,println(ok),B),
  new_color(0.5,0,0.5,Color),
  set_bg(F,Color),
  println(B),
  show(F).
  
dtest:-  
  dialog('Do you want to continue?',D),
  println(D).

ctest:-
  new_frame(F),
  new_console(F,C),
  show(F),
  println(C).
 
etest:-
  new_frame(F),
  new_editor(F,E),
  show(F),
  println(E).
  
%:-[vtetris].
%:-[vq8].

%:-['../unported/gui_lib.pl'].

 
 htest:- 
   for(_,1,3),
     bg((in(a(X)),println(a(X)))),
   fail.
 htest:-  
   for(I,1,3),
     out(a(I)),
   fail.
 

call_gui_method(MethodAndArgs):-call_gui_method(MethodAndArgs,_).

call_gui_method(MethodAndArgs,Result):-
   call_java_class_method(
     'prolog.core.GuiBuiltins',
      MethodAndArgs,
      Result
   ).

show(Container):-call_gui_method(show(Container)).
  
resize(Component,H,V):-call_gui_method(resize(Component,H,V)).

new_frame(Title,Layout,Frame):-
   new_gui_container('new_frame',Title,Layout,Frame).

new_panel(Title,Layout,Frame):-
   new_gui_container('new_panel',Title,Layout,Frame).

new_label(Parent,Name,Label):-
   new_gui_component('new_label'(Parent,Name),Label).

set_label(Label,Text):-
   invoke_java_method(Label,setText(Text),_).
   
   
to_layout(grid(X,Y),grid,X,Y):-!.
to_layout(L,L,0,0).

new_gui_container(Builder,Title,Layout,Container):-
   new_java_class('prolog.core.GuiBuiltins',C),
   to_layout(Layout,L,X,Y),
   invoke_java_method(C,Builder,args(Title,L,X,Y),Container).

new_gui_component(MethodAndArgs,Component):-
   call_java_class_method(
     'prolog.core.GuiBuiltins',
      MethodAndArgs,
      Component
   ).


go:-
  new_frame(foo,grid(2,2),F),
  resize(F,300,300),
  new_panel(F,flow,P),
  new_label(P,hello,L),
  set_label(L,bye),
  println(L),
  println(F),
  show(F),
  println(F).

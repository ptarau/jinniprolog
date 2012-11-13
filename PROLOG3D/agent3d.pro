:-[agent].
:-['/bin/prolog3d.pro'].

agent3d:-
  %broker:start,
  G=run_agent,
  agent(G,prolog3d).

run_agent:-
  reuse_top_window(yes),
  set_bg(''),
  color_bg(0,0,0.2),
  set_max_text(80),
  new_world(U),
  show_world(U),
  world<=U.

xshow(F,X):-functor(X,'$object',1),!,xshow_cat(F,X).
xshow(F,X):-term_to_cat(X,C),xshow_cat(F,C).

gshow(X):-xshow(model_graph,X).
tshow(X):-xshow(draw_tree,X).

xshow_cat(F,C):-
  world=>U0,
  stop_world(U0),
  new_world(U),
  world<=U,
  call(F,U,400,C),
  show_world(U).

% deprecated
tshow_term(T):-gshow(T).
tshow_cat(C):-gshow(C).

  
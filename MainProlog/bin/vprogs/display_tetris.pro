%  Hor Vert Size  - starts at 0
:-[visual_tetris].

% to be obtained differently - currently set in tetris.pl

dims(20,10).

go:-tetris_server.

tetris_server:-
  is_prolog(Prolog),
  prolog_action(Prolog,Action),
  call(Action).
tetris_server:-
  println('GUI unavailable. Please run this with Prolog!').
  
prolog_action(jinni_compiled,run_server).
prolog_action(jinni_interpreted,run_server).

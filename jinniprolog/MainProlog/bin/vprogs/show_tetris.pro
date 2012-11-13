:-[tetris_trace].

dims(20,10).

go:-tetris_server.

tetris_server:-
  is_prolog(Prolog),
  prolog_action(Prolog,Action),
  call(Action).
tetris_server:-
  println('Please run this with BinProlog!').
  
prolog_action(jinni_compiled,run_server).
prolog_action(jinni_interpreted,run_server).
prolog_action(binprolog,run_server).


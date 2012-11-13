go:-
  write_chars("> "),flush,
  println(here_reading),
  read_line(L),
  println(finished_reading=L),
  L\=='',
  atom_codes(L,ICs),
  write_chars(ICs),nl.
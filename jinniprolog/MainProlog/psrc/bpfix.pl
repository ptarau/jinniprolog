% gets rid of some unused operators

:-op(0,fx,(dynamic)).
:-op(0,fx,(mode)).
:-op(0,fx,(module)).
:-op(0,fx,(public)).
:-op(0,fx,(memo)).
:-op(0,fx,(type)).
:-op(0,fx,(delphi)).
:-op(0,fx,(mod)).
:-op(0,fx,(extends)).
:-op(0,fx,(with)).
:-op(800,xfy,(':')).
:-op(700,xfx,('#=>')).
:-op(700,xfx,('<=#')).
:-op(700,xfx,('<==')).
:-op(700,xfx,('==>')).
:-op(700,xfx,(is_bigint)).
:-op(700,xfx,(is_bigdec)).

% add whatever is missing

translate_one_file(Fname,Mode,Printer):-
  find_file(Fname,F),
  seeing(F0),see(F),
  repeat,
  read_clause(C),
  translate(C,Mode,Printer),
  !,
  seen,see(F0).

% add_instr(1,Op,Reg,F,N):-put(Op),put(Reg),put(N),cwrite(F),put(0).

csep:-cnl. %cwrite(',').

add_instr(1,Op,Reg,F,N):-
  cwrite(Op),csep,
  cwrite(Reg),csep,
  cwrite(N),csep,
  cwrite(F),cnl.
  

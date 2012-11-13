% compiler interface

[File|Files]:-compile([File|Files]).

compile(Files):-
  set_global_prop(user_files,Files),
  class_name(prolog)-> recompile(Files)
; Files=[File], (File : toplevel).

co:-get_global_prop(user_files,Files),'$null'\==Files,compile(Files).

recompile:-class_name(File),recompile(File).
recompile(Files):-rollback,mcompile(Files).
 
mcompile(Files):-mcompile(Files,mem).
mdebug(Files):-mcompile(Files,debug).
qcompile(Files):-qcompile(Files,mem).
qdebug(Files):-qcompile(Files,debug).

clean_compile(Files):-
  survive_cleanup(Files,Fs),
  mcompile(Fs),
  abort.
   
% implementation
   
mcompile(Files,Mode):-mcompile(Files,Mode,ttyprint).

mcompile(Files,Mode,Printer):-
  ctime(T1),
  call(Printer,begin_compiling(to(Mode),Files)),
  qcompile(Files,Mode,Printer),
  ctime(T2),
  Time is (T2-T1),
  call(Printer,end_compiling(time(Time,msec),Files)).
 
qcompile(Files,Mode):-  
  qcompile(Files,Mode,eq(_)).
  
qcompile(Files,Mode,Printer):-
  set_global_prop(user_files,Files),
  advance_code_top, % allows to call qcompile repeatedly
  jcompile(Files,Mode,Printer).
    
translate_one_file(Fname,Mode,Printer):-
  term_of(Fname,C),
  translate(C,Mode,Printer),
  fail.
translate_one_file(_,_,_).

scompile(String):-
  rollback,
  scompile(String,mem,ttyprint).

scompile(String,Mode,Printer):-
  translate_one_string(String,Mode,Printer),
  jterminate(Mode).

jterminate(Mode):-jterminate(['$string'],Mode).
 
translate_one_string(String,Mode,Printer):-
  forall(
     sread_terms(String,C,_Vs),
     translate(C,Mode,Printer)
  ).
  
/*
restart: resets code, hastable, syms as after loading wam.bp
rollback: resets code, hastable as after loading wam.bp but NOT syms
commit: sets codeBackTop to current codeTop - pushes last module
        into the kernel

Note: doing a rollback or restart after commit will result in (harmless?)
      growth of code area - but it really does not make sense

Note: they are LOCAL to each Prolog
*/

rollback:-
 jcall(rollback,any,_).

advance_code_top:-
  jcall(advance_code_top,any,_).
    
survive_cleanup(T,NewT):-
  term_codes(T,Cs),
  restart,
  term_codes(NewT,Cs).


/*
File compiler and bootstrapping interface.
*/

javafy:-
 new_java_class('prolog.core.Javafier',C),
 invoke_java_method(C,C,run,void,_).

jboot:-
  fcompile('prolog.pl','../bin/wam.bp').

lboot:-
  fcompile('all_logic.pl','../bin/wam.bp').
  
bincompile(Fs):-fcompile(bin,Fs,'bin.tmp',ttyprint).

asmcc(C):-
  cc(asm,C).

asmcompile(Fs):-
  fcompile(asm,Fs,'asm.txt',ttyprint).

fcompile(Project,Target):-
  fcompile(wam,[Project],Target,ttyprint).
  
fcompile(Mode,Fs,OutFile,Printer):-
  call(Printer,compile_to(Mode,Fs)),
  ctime(T1),
  xcompile(Mode,Fs,OutFile,Printer),
  ctime(T2),
  T is T2-T1,
  call(Printer,total_compile_time(T)).

xcompile(Mode,InFiles,OutFile,Printer):-
  telling(CF),
  tell(OutFile),
  cc_bbuiltins(Mode),
  jcompile(InFiles,Mode,Printer),
  told,
  tell(CF).

lcompile(ProjectFile):-
  namecat('',ProjectFile,'.bp',Target),
  ucompile([ProjectFile],'/bin/lwam.bp',Target).
  
ucompile(ProjectFile):-
  namecat('',ProjectFile,'.bp',Target),
  ucompile([ProjectFile],'/bin/wam.bp',Target).
  
ucompile(InFiles,BasicFile,OutFile):-
  file2chars(BasicFile,Cs),
  telling(CF),
  tell(OutFile),
  translate_all_files(InFiles,wam,ttyprint),
  forall(member(C,Cs),put(C)),
  % jterminate(InFiles,wam),
  told,
  tell(CF).
  
jcompile(Files,Mode,Printer):-
  translate_all_files(Files,Mode,Printer),
  jterminate(Files,Mode).

jterminate(Files,Mode):-
  swrite(Files,EndName0),
  namecat('$end','',EndName0,EndName),
  convert_mode_flag(Mode,NewMode,LinkFlag),
  terminate_file(NewMode,EndName,LinkFlag).
    
terminate_file(Mode,Dummy,Level):-
    /* compile Dummy seems unnecessary */
	cc(Mode,Dummy),
	emit_code(Mode,[[ii(end,'?',Level,Mode)]]).

convert_mode_flag(mem,Mode,Flag):-!,Mode=mem,Flag=1.
convert_mode_flag(debug,Mode,Flag):-!,Mode=mem,Flag=1.
convert_mode_flag(Mode,Mode,0).

include_file(Fs,Mode,Printer):-
  call(Printer,begin_including(Fs)),
  translate_all_files(Fs,Mode,Printer),
  call(Printer,end_including(Fs)).

translate_all_files(Files,Mode,Printer):-
  nonvar(Files),Files=[_|_],
  !,
  forall(
    member(F,Files),
    translate_one_file(F,Mode,Printer)
  ).
translate_all_files(Fname,Mode,Printer):-
  translate_one_file(Fname,Mode,Printer).
  
% called by translate_one - see mco.pl
  
translate(end_of_file,_,_):-!.
translate(':-'([F|Fs]),Mode,Printer):-!,
  include_file([F|Fs],Mode,Printer),
  fail.
translate(C,Mode,_):-cc(Mode,C),fail.

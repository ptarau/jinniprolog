time_goal(Goal):-ctime(T1),topcall(Goal),ctime(T2),T is T2-T1,println(Goal:time(T)).
time(Goal):-time_goal(Goal). % swi compatible

% statistics

statistics:-
	statistics(Name,Data),
	println(Name=Data),
	fail.
statistics.	

stat:-statistics.

% os interface

get_system_property(Name,Prop):-call_java_class_method('java.lang.System',getProperty(Name),Prop).

set_system_property(Name,Prop):-call_java_class_method('java.lang.System',setProperty(Name,Prop),_).

os_name(OS):-get_system_property('os.name',OS).

get_prolog_version(V):-call_java_class_method('prolog.kernel.Top',getPrologVersion,V).

system(Dir,CmdList):-
  system(Dir,CmdList,Output),
  println(Output).

system(Dir,CmdList,Output):-
  new_process(Dir,CmdList,Process),
  run_process(Process,Output).
  
new_process(DirName,CmdList,Process):-
  length(CmdList,L),
  new_array('java.lang.String',L,CMDs),
  foreach((nth_member(X,CmdList,J),I is J-1),array_set(CMDs,I,X)),
  new_java_object('java.lang.ProcessBuilder'(CMDs),PB),
  new_java_object('java.io.File'(DirName),Dir),
  invoke_java_method(PB,directory(Dir),_),
  to_boolean(true,True),
  invoke_java_method(PB,redirectErrorStream(True),_),
  invoke_java_method(PB,start,Process).
  
run_process(Process,Output):-  
  call_java_class_method('prolog.kernel.JavaIO',runProcess(Process),Output).

system([]):-!.
system([X|Xs]):-!,atom_codes(Command,[X|Xs]),shell(Command).
system(Command):-atomic(Command),shell(Command).

run_command(Cmd,StringResult):-
  call_java_class_method('prolog.kernel.JavaIO',runCommand(Cmd),StringResult).

absolute_file(Rel,Abs):-
  new_java_object('java.io.File'(Rel),File),
  invoke_java_method(File,getCanonicalPath,Abs).

root_dir(D):-absolute_file('/',D).

current_dir(DirName):-absolute_file('.',DirName).

parent_dir(D):-absolute_file('..',D).

find_close_file(F,AbsFile):-
  parent_dir(D),
  dir2one(D,F0),
  namecat(D,'/',F0,AF0),
  ( is_file(D,F0)->F=F0,AF=AF0  
  ; is_directory(D,F0),
    dir_has_file(AF0,F),
    namecat(AF0,'/',F,AF)
  ),
  absolute_file(AF,AbsFile).

dir2files(DirName,Files):-
  findall(F,dir_has_file(DirName,F),Files).

dir2dirs(DirName,Dirs):-
  findall(D,dir_has_dir(DirName,D),Dirs).

% dirs  
dir_has_file(DirName,FileName):-
  dir2one(DirName,FileName),
  is_file(DirName,FileName).

dir_has_dir(DirName,FileName):-
  dir2one(DirName,FileName),
  is_directory(DirName,FileName).
    
dir2one(DirName,FileName):-
  new_java_object('java.io.File'(DirName),Dir),
  invoke_java_method(Dir,list,Array),
  invoke_array_method(getLength(Array),L),
  Max is L-1,
  for(I,0,Max),
  invoke_array_method(get(Array,I),FileName).
 
is_file(InDir,FileName):-
  namecat(InDir,'/',FileName,Name),
  new_java_object('java.io.File'(Name),F),
  invoke_java_method(F,isFile,B),
  is_true(B).
  
is_directory(InDir,FileName):-
  namecat(InDir,'/',FileName,Name),
  new_java_object('java.io.File'(Name),F),
  invoke_java_method(F,isDirectory,B),
  is_true(B).
  
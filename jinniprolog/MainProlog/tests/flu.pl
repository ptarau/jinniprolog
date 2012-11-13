go:-
  [A,B]="ab",
  new_java_class('jinni.agents.FileSink',C),
  new_java_object(C,args('temp.txt'),F),
  invoke_java_method(F,putChar(A),_),
  invoke_java_method(F,putChar(B),_),
  invoke_java_method(F,putObject(hello),_),
  invoke_java_method(F,putObject(bye),_),
  invoke_java_method(F,putByte(32),_),
  invoke_java_method(F,putChar(A),_),
  invoke_java_method(F,putChar(B),_),
  invoke_java_method(F,stop,_),
  file2chars('temp.txt',Cs),
  name(S,Cs),
  println(S).


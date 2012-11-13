package(jgui).

clean:-
  package(P),
  dir2files(P,Fs),
  member(F,Fs),
  ends_with(".class",F),
  fdelete(P,F),
  fail.
clean.  

compile:-clean,package(P),javac(P).

run:-prolog(['mcompile(project)','call(ide)','sleep(100000)']).

jrun:-
  package(P),
  namecat(P,'.Start',Main),
  java([ '-classpath','.;/bin/prolog.jar',Main]).

go:-compile,run.

jgo:-compile,jrun.

% end

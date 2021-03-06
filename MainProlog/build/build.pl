/**
  - handles the (re)building of Prolog with various Java and .NET compilers
  - requires BinProlog - until Prolog gets around Java's 
    crippled process control limitations 
    
    UNTESTED since 2006
*/

% CONSTANTS

topdir(TD):-up('',P),up(P,TD).
rootdir('MainProlog').
exclude('prolog\doc\*').
docdir('doc\jdoc\').
bakname('\bak\pjbak').
srcdir('psrc').
wamfile('wam.bp').
zipfile('prolog.zip').
jarfile('prolog.jar').
bindir('bin').
javadir('prolog\kernel').
javadir1('prolog\core').
javadir2('prolog\logic').
javapac('prolog.kernel').
javapac1('prolog.core').
javapac2('prolog.logic').
netexe('netjinni.exe').
netdll('netjinni.dll').
mainclass(MC):-javapac(PAC),namecat(PAC,'.','Main',MC).
demodir('\jinnidemo\').
netdir('\netjinni\').
pocketdir('\pocketProlog\').
srcdist('dist\prolog.zip').
bindist('dist\p.zip').

% TOOLS

up(To):-up('',To).
up(From,To):-namecat('..','\',From,To).

to_bin(ToBinDir):-bindir(BIN),up(BIN,ToBinDir).

del(File):-del('.',File).
del(Dir,File):-cmd(Dir,['del /Q ',File]).

dir(Pattern):-cmd(['dir ',Pattern]).

currdir(D):-
  % pwd(Cs),
  pcollect('cmd /C cd',Cs),
  findall(C,(member(C,Cs),C=\=10,C=\=13),Ds),
  atom_codes(D,Ds).

cmd(Task):-cmd('.',Task).

cmd(Dir,Task):-
   if(atomic(Task),List=[Task],List=Task),
   currdir(Pwd),
   make_cmd(List,Cmd),
   ( cd(Dir)->
     currdir(NewDir),
     println([dir=NewDir,cmd=Cmd]),
     system(Cmd)
   ; println(['bad directory',Dir])
   ),
   cd(Pwd).

/* collects and compress text files - with LF terminated lines */

azip(Dir,ZipFile,Root,Exclude,Pattern):-
   cmd(Dir,['zip.exe -9 -ll -r ',ZipFile,' ',Root,' -x ',Exclude,' -i ',Pattern]).

azip(Dir,ZipFile,Root):-
  cmd(Dir,['zip.exe -9 -ll -r ',ZipFile,' ',Root]).


/* collects and compress binary files */

bzip(Dir,ZipFile,Root,Exclude,Pattern):-
   cmd(Dir,['zip.exe -9 -r ',ZipFile,' ',Root,' -x ',Exclude,' -i ',Pattern]).   


bzip(Dir,ZipFile,Root):-
   cmd(Dir,['zip.exe -9 -r ',ZipFile,' ',Root]).   

   
/* source backup */

jbak:-jbak(last).

jbak(Version):-
  topdir(TopDir),
  rootdir(Root),
  exclude(Exclude),
  make_bakfile(Version,BakFile),
  del(BakFile),
  println(creating_new(BakFile)),
  map(
    azip(TopDir,BakFile,Root,Exclude),
    ['*.java','*.txt','*.pl','*.html','*.htm','*.lnk','*.pro','*.bat']
  ),
  map(
    bzip(TopDir,BakFile,Root,Exclude),
    ['*.sln',"*.jsl",'*.vjsproj','*.vjsproj.user','*.vjp']
  ),
  dir(BakFile).

make_bakfile(Version,BakFile):-bakname(File),make_cmd([File,Version,'.zip'],BakFile).

/* compiles Prolog psrc files */

jboot:-
  currdir(CD),
  up('',UP),
  cd(UP),
  srcdir(SRC),
  bindir(BIN),
  wamfile(File),
  del(BIN,File),
  cmd(SRC,'bp bpco.pl call(and(jboot,halt))'),
  % added as \bin\wam.bp is default for loading
  make_cmd(["copy /Y ",BIN,"\",File," \bin"],Cmd),
  println(doing=Cmd),
  system(Cmd),
  cd(CD),
  makebincomp.

makebincomp:-
  currdir(CD),
  up('',UP),
  cd(UP),
  srcdir(SRC),
  bindir(BIN),
  swrite(and(fcompile(bpco),halt),ToDo),
  cmd(SRC,['bp ',ToDo]),
  up(BIN,UBIN),
  namecat(UBIN,'/','bpco.wam',WF),
  namecat('copy',' bpco.wam ',WF,Copy),
  cmd(SRC,Copy),
  del(SRC,'*.wam'),
  cd(CD).

ucompile(Project):-
  jboot,
  currdir(CD),
  up('',UP),
  cd(UP),
  srcdir(SRC),
  swrite(and(ucompile(Project),halt),ToDo),
  cmd(SRC,['bp bpco.pl ',ToDo]),
  cd(CD).

unjavafy:-
  currdir(CD),
  del(CD,'JMain.class'),
  up('',UP),cd(UP),
  javadir(JDIR),del(JDIR,'Wam*.java'),
  cd(CD).

javafy:-
  javafy('').

default_jtarget('1.5'). /* 1.5 in both and 1.6 also works */

jtarget(T):-asserted(ojtarget(T)),!.
jtarget(T):-default_jtarget(T).

target_cmd(Before,After,TC):-jtarget(T),make_cmd([Before,' -source ',T,' -target ',T,' ',After],TC).
  
javafy(ByteCodeFile):-
  jboot,
  unjavafy,
  currdir(CD),
  up('',UP),cd(UP),
  target_cmd('javac -J-Xmx1024M','-O -g -classpath . -d bin build/JMain.java',TC),
  cmd('./',TC),
  bindir(BIN),
  namecat('java -Xmx512M -cp . build.JMain',' ',ByteCodeFile,Cmd),
  println(here=Cmd),
  cmd(BIN,Cmd),
  wamfile(WAM),
  del(BIN,WAM),
  cd(CD).
  
/* compiles with Sun's java */

jcompile:-
  target_cmd('javac -J-Xmx1024M','-O -g -classpath . -d ',TC),
  compile_cmd(TC).

/* runs with Sun's Java */

jc:-run_cmd('java -classpath . ').  

/* runs with Personal Java - if installed */

pjc:-run_cmd('pjava -classpath . ').

/* runs with J++ jview */

mjc:-to_bin(Dir),cmd(Dir,'mjinni.exe').
    
jclean:-
  unjavafy,
  to_bin(UBIN),
  javadir(ClassDir),killdir(UBIN,ClassDir),
  javadir1(ClassDir1),killdir(UBIN,ClassDir1),
  javadir2(ClassDir2),killdir(UBIN,ClassDir2),
  cmd(UBIN,'cmd /C rmdir prolog'),
  wamfile(WAM),
  del(UBIN,WAM),
  del(UBIN,'bpco.wam'),
  zipfile(ZIP),
  del(UBIN,ZIP),
  jarfile(JAR),
  del(UBIN,JAR),
  % netexe(EXE),
  del(UBIN,"*.exe"),
  del(UBIN,"*.bp"),
  del('..','*.pdb'),
  % del('..','*.jsl'), 
  docdir(JDOC),up(UpDir),
  killdir(UpDir,JDOC),
  up(ClassDir,JSRC),
  del(JSRC,'*.class'),
  killdir('..',obj).
  
killdir(UpDir,KilledDir):-cmd(UpDir,['cmd /C rmdir /Q /S ',KilledDir]).

compile_cmd(JC):-
  currdir(CD),
  up('',UP),
  cd(UP),bindir(BIN),
  
  javadir(JAVA),del(JAVA,'*.class'),
  cmd([JC, BIN, ' ', JAVA,'\*.java']),
   
  javadir1(JAVA1),del(JAVA1,'*.class'),
  cmd([JC, BIN, ' ', JAVA1,'\*.java']),
  
  javadir2(JAVA2),del(JAVA2,'*.class'),
  cmd([JC, BIN, ' ', JAVA2,'\*.java']),
  
  cd(CD).

run_cmd(Java):-
  to_bin(Dir),
  mainclass(M),
  cmd(Dir,[Java,M]).  
  
/* compiles for Microsoft's .NET */
  
ncompile:-
  up('',UP),
  bindir(BIN),
  netexe(EXE),
  namecat(BIN,'\',EXE,OUTFILE),
  mainclass(MAIN),
  cmd(UP,['vjc /o+ /w:4 /out:',OUTFILE,' /t:exe /recurse:*.java /m:',MAIN]).

to_dll:-
  up('',UP),
  bindir(BIN),
  netdll(EXE),
  namecat(BIN,'\',EXE,OUTFILE),
  % mainclass(MAIN),
  cmd(UP,['vjc /o+ /w:4 /out:',OUTFILE,' /t:library /recurse:*.java']).
  
/* runs unde .NET Framework with J# */

njc:-
  to_bin(Dir),
  netexe(EXE),
  cmd(Dir,EXE).  

/* makes class documentation using javadoc */

makedoc:-
  up(UPDIR),
  docdir(JDOC),
  javapac(JAVASRC),
  javapac1(JAVASRC1),
  javapac2(JAVASRC2),
  cmd(UPDIR,['cmd /C del /Q /S ',JDOC]),
  cmd(UPDIR,[mkdir,' ',JDOC]),
  cmd(UPDIR,['javadoc -public -use -author -d ',
      JDOC,
      ' -windowtitle "Prolog in Java: Jinni Class Documentation" ',
      JAVASRC,
      ' ',
      JAVASRC1,
      ' ',
      JAVASRC2
  ]).

/* compiles with java compiler and packs to ZIP file */
  
smallzip:-
  jcompile,
  
  to_bin(Dir),
  zipfile(ZF),
  del(Dir,ZF),

  javadir(KERNEL),
 
  bzip(Dir,ZF,KERNEL,'*.txt','*.class'),
 
  killdir(Dir,KERNEL),
  
  javadir1(KERNEL1),
  bzip(Dir,ZF,KERNEL1,'*.txt','*.class'),
  killdir(Dir,KERNEL1),
  
  javadir2(KERNEL2),
  bzip(Dir,ZF,KERNEL2,'*.txt','*.class'),
  killdir(Dir,KERNEL2).

makezip:-
  smallzip,
  jboot,  
  wam2zip.

/* compiles with J++ and packs to JAR file */

smalljar:-
  jcompile,
  to_bin(Dir),
  jarfile(JF),del(Dir,JF),
  javadir(KERNEL),namecat(KERNEL,'\','*.class',Pattern),
  javadir1(KERNEL1),namecat(KERNEL1,'\','*.class',Pattern1),
  javadir2(KERNEL2),namecat(KERNEL2,'\','*.class',Pattern2),
  cmd(Dir,['jar cvfm ',JF, ' manifest.txt ',Pattern,' ',Pattern1,' ',Pattern2
  % ,' ',Classlib
  ]),
  killdir(Dir,KERNEL),
  killdir(Dir,KERNEL1),
  killdir(Dir,KERNEL2).

jarallprogs:-
  addtojar_all(classlib), % recursively !
  addtojar_all(agentlib), % recursively !
  addtojar(vprogs),
  addtojar(progs).
  
addtojar:-
  addtojar_all(classlib),
  addtojar_all(agentlib).

addtojar(ProgDir):-
  addtojar(ProgDir,'.pl'),
  addtojar(ProgDir,'.pro').

addtojar(ProgDir,Suf):-
  to_bin(Dir),
  jarfile(JF),
  namecat(ProgDir,'/*',Suf,Lib),
  cmd(Dir,['jar uvf ',JF,' ',Lib]).

% recurses over ProgDir
addtojar_all(ProgDir):-
  to_bin(Dir),
  jarfile(JF),
  cmd(Dir,['jar uvf ',JF,' ',ProgDir]).
    
makewjar:-
  smalljar,
  jboot,
  to_bin(Dir),
  jarfile(JF),
  wamfile(WAM),
  cmd(Dir,['jar uvf ',JF,' ',WAM]),
  del(Dir,WAM).

makejar:-
  makejar('').

makejar(ByteCodeFile):-
  javafy(ByteCodeFile),
  smalljar,
  unjavafy.

wam2zip:-
  to_bin(Dir),
  zipfile(ZF),
  wamfile(WAM),
  bzip(Dir,ZF,WAM),
  del(Dir,WAM).
  
pl2zip:-
  to_bin(Dir),
  map(azip(Dir,'prolog.zip'),[
    classlib,
    progs,
    vprogs
  ]).

/* high level scripts for deployment */
      
makeall:-
  jclean,
  makezip,
  javafy,
  smalljar,
  ncompile,
  unjavafy,
  jarallprogs,
  makebincomp,
  makedoc,
  jboot.
  
makedemo:-
  jclean,
  % javafy,smalljar,
  makebigjar,
  to_bin(Dir),
  demodir(Demo),
  killdir('.',Demo),
  del(Dir,'Release\netjinni.exe'),
  cmd(['xcopy /K /S ',Dir,' ',Demo]),
  del(Demo,'*.lnk'),
  del(Demo,'std*.txt').
 
makenet:-
  jclean,
  javafy,
  ncompile,
  to_dll,
  % jboot,
  to_bin(Dir),
  netdir(Net),
  killdir('.',Net),
  cmd(['xcopy /K /S ',Dir,' ',Net]),
  del(Net,'*.lnk'),
  del(Net,'*.bat'),
  del(Net,'Demo*.html'),
  cmd(['copy ',Dir,'\nbm.bat ',Net]),
  cmd(['copy ',Dir,'\nj.bat ',Net]),
  demodir(Demo),
  cmd(['copy ',Net,'\netjinni.exe ',Demo]).

makebigjar:-
  jclean,
  makejar,
  jarallprogs.
  
makepocket:-
  assert(ojtarget('1.2')),
  makebigjar,
  to_bin(Dir),
  pocketdir(Pocket),
  killdir('.',Pocket),
  cmd(['xcopy ',Dir,'\*.* ',Pocket]),
  del(Pocket,'*.bat'),
  cmd(['copy ',Dir,'\mjc.bat ',Pocket]),
  del(Pocket,'Demo*.html'),
  retractall(ojtarget(_)).
 
makedist:-
  makeprof.


makesrc:-
  jclean,
  cmd('copy \bin\bp.exe .'),
  cmd('copy \bin\zip.exe .'),
  cmd('copy \bin\unzip.exe .'),
  up('',U1),
  up(U1,U2),
  srcdist(DirZip),
  del(U2,DirZip),
  bzip(U2,DirZip,prolog),
  del('.','bp.exe'),
  del('.','zip.exe'),
  del('.','unzip.exe').

makeprof:-
  makeall,
  up('',U1),
  up(U1,U2),
  bindist(DirZip),
  del(U2,DirZip),
  bzip(U2,DirZip,'prolog\bin'),
  bzip(U2,DirZip,'prolog\doc'),
  bzip(U2,DirZip,'prolog\README.txt').
  
  
/* script for development */
go:-
  jclean,
  jboot,
  mcompile,
  % smalljar,jarallprogs,
  mjc.
  
makeweb:-
  jclean,
  makejar.
    
makewnet:-
  jclean,
  smalljar,
  jboot.
  

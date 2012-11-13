del /Q \bin\prolog.jar
dir \bin\prolog.jar
del /Q \bin\prolog.bat
bp.exe build call(makebigjar) call(halt)
copy ..\bin\prolog.jar \bin
copy ..\bin\prolog.bat \bin
copy ..\bin\JinniUserGuide.html \bin
bp.exe build call(jboot) call(halt)
copy ..\bin\wam.bp \bin
copy ..\bin\prolog.bat \bin
copy ..\bin\bpco.wam \bin
bp.exe build call(jclean) call(halt)



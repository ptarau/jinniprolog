cls
del /Q \bin\prolog.jar
cd\Users\tarau\Desktop\go\workspace\MainProlog\build
CALL jinstall.bat
cd ..\..\RLI
CALL compile.bat
cd ..\JGUI
CALL export.bat
cd ..\AGUI
CALL export.bat
cd ..\MainProlog\build
dir \bin\prolog.jar
copy makemainprolog.bat \bin

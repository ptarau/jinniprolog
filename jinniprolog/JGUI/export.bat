CALL compile.bat
copy \bin\prolog.jar .
jar -uvf prolog.jar jgui/*.class
copy prolog.jar \bin
del /Q prolog.jar

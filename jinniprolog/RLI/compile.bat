CALL clean.bat
javac -classpath ".;/bin/prolog.jar" rli/*.java
REM jar -cvf rli.jar rli/*.class
copy \bin\prolog.jar .
jar -uvf prolog.jar rli/*.class
copy prolog.jar \bin
del /Q prolog.jar


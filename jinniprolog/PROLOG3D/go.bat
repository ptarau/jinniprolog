CALL compile.bat
REM CALL prolog3d.bat %1 %2 %3 %4 %5
java -Xmx512M -classpath ".;/bin/prolog.jar;j3d-vrml97.jar" prolog.kernel.Main "qcompile(prolog3d)" %1 %2 %3 %4 %5 %6

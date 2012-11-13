CALL compile.bat
java -Xmx512M -classpath "/bin/prolog.jar;.;vrml97.jar" prolog.kernel.Main "qcompile(prolog3d)" %1 %2 %3 %4 %5 %6

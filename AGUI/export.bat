javac -classpath "/bin/prolog.jar;." agentgui/*.java
copy \bin\prolog.jar .
jar -uvf prolog.jar agentgui/*.class
copy prolog.jar \bin
del /Q prolog.jar

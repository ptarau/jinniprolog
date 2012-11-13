javac -classpath ".;/bin/prolog.jar" agentgui/*.java
CALL prolog.bat "call_java_class_method('agentgui.Main',startgui,_)" %1 %2 %3 %4
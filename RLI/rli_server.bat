REM java -classpath ".;/bin/prolog.jar" -Djava.rmi.server.codebase=file:C:/tarau/xprolog/rli/ rli.RLIServer %1 %2 %3

java -classpath "/bin/prolog.jar" -Djava.rmi.server.codebase=file:C:/bin/prolog.jar rli.RLIAdaptor server %1 %2 %3
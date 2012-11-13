rem CALL cleanprolog.bat
javac -O -g -nowarn -d bin prolog/logic/*.java
javac -O -g -nowarn -d bin prolog/kernel/*.java
javac -O -g -nowarn -d bin prolog/core/*.java
cd psrc
bp bpco.pl call(and(jboot,halt))
cd ..\bin
REM java -cp . prolog.kernel.Main wam.bp "and(jboot,halt)"
REM java -cp . prolog.kernel.Main wam.bp "[nrev]" "and(big,halt)"
java -cp . prolog.kernel.Main wam.bp
cd ..

javac -O -g -nowarn -d bin prolog/logic/*.java
cd psrc
bp bpco.pl call(and(lboot,halt))
cd ..\bin
copy wam.bp \bin\lwam.bp 
java prolog.logic.Start \bin\lwam.bp
cd ..

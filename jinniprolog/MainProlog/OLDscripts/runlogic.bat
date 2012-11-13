cd psrc
bp bpco.pl "call(and(lcompile('..\bin\progs\%1'),halt))"
cd ..
cd bin
java prolog.logic.Start "progs\%1.bp" %2
cd ..


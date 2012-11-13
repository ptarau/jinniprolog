del /Q psrc\logic\*.wam
del /Q psrc\kernel\*.wam
del /Q psrc\core\*.wam

cd bin
del wam.bp
cd prolog

cd logic
del /Q *.class
cd ..
rd logic

cd kernel
del /Q *.class
cd ..
rd kernel

cd core
del /Q *.class
cd ..
rd core

cd ..
rd prolog
cd ..

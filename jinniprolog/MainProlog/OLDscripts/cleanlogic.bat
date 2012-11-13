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

cd ..
cd ..

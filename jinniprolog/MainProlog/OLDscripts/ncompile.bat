del /Q ..\bin\netjinni.exe
cd ..
vjc /o+ /w:4 /out:bin\netjinni.exe /t:exe /recurse:*.java /m:prolog.kernel.Main
cd build

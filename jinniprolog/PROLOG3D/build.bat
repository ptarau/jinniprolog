@echo type jinstal in prolog/build before running this
CALL compile.bat
rmdir /S /Q temp3d_
mkdir temp3d_
jar cvf temp3d_/this.jar prolog3d/*.class *.pro *.pl
cd temp3d_
copy \bin\prolog.jar .
copy \bin\prolog2d.jar .
copy \bin\prolog2d.pro .
copy ..\j3d-vrml97.jar .
CALL mjar.bat prolog3d prolog2d j3d-vrml97 this prolog
copy prolog3d.jar \bin\prolog.jar
cd ..
rmdir /S /Q temp3d_
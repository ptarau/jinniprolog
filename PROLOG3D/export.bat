CALL compile.bat
CALL wrap.bat
CALL makedoc
copy \bin\prolog3d.pro \jinnidemo
text2html \bin\prolog3d.pro \jinnidemo\code.html
copy \bin\prolog3d.jar \jinnidemo
copy j3d-vrml97.jar \jinnidemo
copy *.html \jinnidemo
copy *.jpg \jinnidemo
copy *.png \jinnidemo
copy *.obj \jinnidemo
copy prolog3d.bat \jinnidemo
copy demo3d.bat \jinnidemo
copy jdemo3d.bat \jinnidemo
mkdir \jinnidemo\models
copy \paul\models\*.* \jinnidemo\models
copy *.java \jinnidemo
copy test.bat \jinnidemo
copy prolog3d\Prolog3D.java \jinnidemo\Prolog3D.txt
mkdir \jinnidemo\doc3d
del /Q /S \jinnidemo\doc3d\*.*
xcopy /Q /S /E doc3d \jinnidemo\doc3d

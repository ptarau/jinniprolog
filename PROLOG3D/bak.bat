del /Q \bak\p3dbak%1.zip
zip.exe \bak\p3dbak%1.zip -9 -ll *.java prolog3d\*.java *.bat *.txt *.pl *.pro
zip.exe \bak\p3dbak%1.zip -9 *.sln *.vjp *.vjsproj *.jpg
ls -t -r c:/bak/p3dbak*.zip|tail -5
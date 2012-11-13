del /Q temp.pro
zip -9 -ll -r rli_%1.zip *.pro *.bat *.txt rli\*.java
zip -9 -r rli_%1.zip *.sln *.vjsproj *.user
copy rli_%1.zip bak
del /Q rli_%1.zip
dir bak\rli_*.zip
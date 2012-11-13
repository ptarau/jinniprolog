zip -9 -ll -r comb_%1.zip *.pro *.bat *.txt
zip -9 -r comb_%1.zip *.sln *.vjsproj *.user
copy comb_%1.zip bak
del /Q comb_%1.zip
dir bak\comb_*.zip

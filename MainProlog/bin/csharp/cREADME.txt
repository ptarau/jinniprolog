First download the .NET edition and unzip it. Check that the directory contains netjinni.dll which is needed to call Prolog from C#.

Note that the netjinni.dll is now
included also in the otherwise Java based jinnidemo.zip.
 
The *.sln project assumes a reference to directory
c:\netjinni\netjinni.dll. Please edit this if you
have netjinni.dll at another location. After compiling
your C# program in the Visual Studion .NET 2003 or newer
IDE, you can test it by typing

run.bat

in this directory, or by calling directly:

bin\Debug>PrologAdaptor.exe

To call C# or VB methods include the as references,
if needed, and then use Jinni's reflection based interface to call them as if they were J# methods. You might want
to build a J# layer calling C#  or VB methods,
to have allow checking of your calls at compile
time.

Paul Tarau
BinNet Corp.

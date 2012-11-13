RELEVANT TO HOLDERS OF Jinni Prolog Full Source License

Welcome to Jinni Prolog Full Source Edition!

Please read our licensing conditions in file LICENSING.html

What's new in Jinni Prolog

Jinni Prolog is implemented using very fast, reentrant, WAM based engines
and a flexible Reflection based Java interface giving to Prolog
programmers access to zillions of lines of Java and .NET
components.

Jinni Prolog provides a powerful Reflection based Java interface,
supports multiple Prolog databases and an elegant Object 
Oriented Prolog layer. A GUI, multi-threading, blackboards and
networking operations make Jinni Prolog a complete
solution for building client-server and 3-tier rule based 
program components.

Still, the the look and feel of basic Prolog and its syntax 
are preserved. Jinni Prolog gives a glimpse at how a next 
generation knowledge processing languages can look - despite
its compact design it  has features going far beyond ISO Prolog.
Jinni Prolog contains embedded Web server and Web tunelling
for quick deployment of you Prolog solution as Web services.

Jinni Prolog sees the Web as if it were a local files system.
It can read files directly from URLs and zipped directories.
It can read and write directly from Java strings launch multiple
threads, handles multiple dynamic databases etc.
 
For more information on Jinni, take a look at the
JinniUserGuide.html our Web based demos, the 
bin/JinniUserGude.html file, and the documentation
in directory doc.

Key Source Modules

+---bin -- contains Jinni runtime prolog.jar, applets and demos
¦   +---classlib - Object Oriented Prolog classes
|   +---agentlib - Agent Oriented Prolog classes
¦   +---JavaCallsJinniAndJinniCallsBack -- 
¦   +---prolog/kernel+prolog/core - Java class files
¦   +---progs  -- example programs
¦   +---vprogs -- visual programs using GUI components
+---build -- build.pl contains BinProlog scrips for generating all components
+---doc
¦   +---jdoc - java source documentation (javadoc)
¦   +---papers -- technical papers about Jinni and its applications
¦   +---tutorial -- introduction Prolog programming with Jinni
+---jinni - jinni source files
¦   +---core   -- other core Jinni components (networking, HTTP server, RPC)
¦   +---kernel -- Java based BinWAM emulator, engines, multi-threading
+---psrc -- Prolog sources of compiler and libraries
         -- prolog.pl includes the list of Prolog files
         -- bin/wam.bp contains the result of compiling prolog.pl

Look in build/build.pl and various batchfiles and in the Visual J++ project files
for work with the sources. The provided BinProlog bp.exe will regenerate aeverything
by running 

  makeall.bat (or ?-makeall. at the "bp.exe build" Prolog prompt ).

This assumes you have Java, .NET J# and J++ installed and
zip.exe, unzip.exe, javac.exe (Sun's Java compiler), jvc.exe (J++ compiler) 
and vjc.exe (J# compiler) in your PATH. (All tested on Windows XP).

  jcompile.bat will regenerate the Java classes only

To bootstrap Jinni (have it recompile its own Prolog kernel wam.bp) 
go in psrc and run mjc.bat or jc.bat, then type jboot.

Try out appletgo.bat (for applets) and bm.bat (for benchmarking with jview).

To generate a PocketPC version type

 makepocket.bat

then copy to the iPAQ's File Store - locate and click on pprolog.lnk
- Jinni should run fine. Edit pprolog.lnk to change location information
on the PocketPC. Jinni Prolog uses a fast Personal Java compatible
emulator (running on virtually any Java platform - Pocket PCs (iPAQ
with Jeode runtime) included.

Enjoy,

BinNet Corporation
http://www.binnecorp.com


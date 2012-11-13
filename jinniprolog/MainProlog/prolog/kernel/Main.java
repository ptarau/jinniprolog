package prolog.kernel;

/*
<pre>
<i>USAGE:<i>
<b>java -cp [prolog.jar or .] prolog.kernel.Main [wam.bp or prolog.jar jinni.jc or path_to_jinni_root] [goals]
</b>
 
This is the main entry point for console usage of prolog.kernel.

The *.bp or *.jc file is the name of a compiled Prolog file which
contains the startup code for the runtime interpreter.

Note: Non-console users can invoke Machine directly see
examples in bin\JavaCallsPrologAndPrologCallsBack
 * 4.6 - interoperating with new prolog - added newcall(Goal), newtop
 * 13.44: reorganized as BinPrologJ
 * 12.71: high risk change in CodeStore - loader ignores clauses which are not grouped, rather than raising exception
 * 12.3x-12.4x extensive refactoring to ensure separation of
 *   prolog.logic: the logic engine
 *   prolog.kernel: reflection, IO, parsing/printing
 *   prolog.core: others: code packaging, graph libraries, GUI, XML, networking/communication etc.
 * 
 * 12.2x-11.3x extensive refactoring to eliminate
 * slowdown due to large case statment in LogicEngine to
 * which Java 1.6.0 showed unusual sensitivity. Accelerated
 * by more than an order of magnitude onder java 1.60
    LAST CHANGES FOR THIS VERSION: in LIFO order
    11.15 - added call_by_association as an extension to Hilog syntax
    11.15 - added Closure@(Arg1,...,ArgN) Hilog syntax 
    11.10 - added flexible file finding capabilities
    11.05 - added global_set,global_get - independent of name spaces local to a class
    10.?? - moved term_hash to java
    10.42 - added \(X) as complement of X, made /(A,B,C) faster when A or B are already float
    10.39 - added new_array,array_set,array_get - see refl.pl
    10.38 Noticed bug: / char breaks command line char processing.
          Fixed - now args[0] if "(" and ")" are present is not confused 
          for a file command even if it has "/" or "\" in it.
          Together with the add_to_path(dir) which now appends / to the end of dir
          if missing, ensures that dynamic extension of path from command line
          works properly.
    10.33 added object2xml/2 and xml2object for serializing beans to XML files and read them back
    10.32 added set_format_precision(int N) - local to a given machine
          to control how many digits are displayed for floating point data
    10.31 added xref - try it with: undefs(File)
    10.29 released as demo on June 3 2004 - also including Jinni3D demo
    10.23 - fixed bug in Cat.java when adding vertex before morphism
            coming from the change Cat extends RankedGraph
            Cat.java implements finite categories that can also
            be seen as graphs with multiple attributes on vertices and edges
    10.22 - added getEdgeCount() in Graph.java
          - fixed bug in ObjectDict - remove() was always returning null
    10.19 - bug in to_boolean(true,B) - to FIX
    10.10 redefined if_defined/2 to also check for dynamic code if compiled code is not defined
          fixed bug in asserted - now it is BinProlog compatible on facts and rules 
    10.0x fixed bug introduced in 9.9x in float point comparison making all floats equal
    9.98 fixed bug in run_server  answer_one_query - 
         deletion of "no" symbol on failing RPC goal
    9.88 Added Stateful to Javafier so that serialization
         and control of serial version is implemented uniformly
    9.87 Added GraphRanker to Graphs, Cat and SmallCat for supporting 
         basic Category Theory - objects and morphisms
         SmallCat uses BitSets for fast implem. of Categories with
         small set of Morphisms     
    9.75 Added Graphs to prolog.core
    9.71 changed queue API and dynamic db ops
    9.70 reordered copy_term to produce for natural order
    9.69 released as demo Jan 18, 2004
    9.68 fixed hkey and added hash_save
    9.60ok added BinProlog's hkey and term_hash to also cover lists 
         and hash_get to list all key/vals when given var key arg
    9.56ok fixed resulting bugs after the change
    9.53 replaced Hashtable with new ObjectDict - also covering symbol table functions
         and keeping insertion order (when no deletions occur)
    9.48 added foreach - to replace forall - which is used with other meaning
    9.47 fixed hash table collision problem for large preds and consec args
         see Dict.hashKeys
    9.46 implemented serialization of class Prolog and/or Machine
    9.40 added val(where,...) to make remote_run run locally
    9.39 added BPAdaptor for TwinProlog
    9.36 updated documentation
    9.35 prompts as L# or Prolog 2004 using System properties
         better exception handling
         released as demo Nov 13
    9.32 fixed memory leak: see eng_test, bg_test, rpc_test
         reorganized exception handling: error messages will always
         get printed out but have approrpiate effects on 
         based on context i.e. shells, server etc.
         note that deterministic catch_once should be used 
         when possible on non-failing goals as catch
         could still leak temporary engine it creates
         
    9.28 added match_more_list_elements and RECURSION_DEPTH control in TermReader
         arbitray lists can now be sent over sockets
         WATCH for impact of compresed list representation on unif instructions etc.
    9.27 fixed bug in read/1 introduced in 9.25
    9.26 made copy_term (xcp) iterative using an IntStack
    9.25 made e Java based shell and remote shell - ability to fully redirect I/O
    9.13 fixed bug in Javafier activate - after changing wam.bp to char file
         added zipping flag for compressing or not bytecode
*   9.11 made automatic conversion to C# possible for prolog.kernel
*        added ability to save password protected fast copy of a db
*        direct ObjectQueue API - faster databbase (10 times)
*        removed inner classes from Dict
*        C# conversion procedure:
         - Open->Convert->conversion assistant Console App
         - Main.cs->main.cs
         - put null to string stream error
         - UTF7->UTF8
         - remove override in Machine.Run
         - Extender - uncomment C# stdio/stdout code
    9.0x? changed wam.bp to UTF8 text file - for portability
*   9.00? separated in packages prolog.kernel + prolog.core 
*   8.94 removed interfaces as a means to store constans - for C# compatibility
*        InstructionIDs (now also containg builtins) is an abstract class
*   8.88 detected bug in notify_about: non-matching data blocks future matching data from triggering
*   8.85 first step to .NET C# port - separated 2 packages : kernel (portable) and core
*   8.74 released demo Sept 17 2003 tested also under .NET 2003
*   8.72 fixed syntax errs with (;) in lists
*   8.70 fixed precision problem with float == Java double
*        fixed f(a)=..[A,B,C] spurious error message
*        added sin,cos,abs,round etc. by reflecting most of java.lang.Math
*   8.67 added setErrmes/1 getErrmes/1 and timed_call/4
*   8.66 added Web based port 9090 tunnel allocator
*   8.61 added TCP tunnel allocator
*   8.60 released as demo May 23,2003
*   8.59 added db_clean/1, db_assertz/2, 
         *.bp args override loading javafied code from jar file
*   8.56 added server_tunnel/2 and client_tunnel/4
*   8.54 added setStdInput(InputStream f) and setStdOutput(OutputStream f)
*   8.52 added dir2files/2 and dir2dirs/2
*   8.50 fixed bug in Transport with reusable sockets - read_from should be synchronized
*   8.49 removed no arg constructor from server - conflicted with inheriting multiple servers
*   8.49 tried out calls from C# under .NET J# - it works!
*   8.48 added support for Prolog Server Agents
*   8.47 added db_save
*   8.47 fixed bug in compare/3, ==, \==: everything was equal to ''
*   8.46 fixed bug in find_most: stop should not be called after engine fails
*   8.42 added POST method handler to http_server
*   8.41 fixed bug in loadInstruction reading terms containing '' of length 0
*   8.40 fixed TokenReader bug with codes_words (quotes cut tokens)
* - 8.38 added rpc package - reusable socket based
         communication with BinProlog and Prolog clients and servers
* - 8.37 Jinni 2003: documented creation of standalone applications and
         addition of embedded http_server
* - 8.31 made '-' single character in the tokenizer
* - 8.31 fixed bug in name/2 with name(X,"a-b") retuning a compound term
* - 8.29 added to_lower_char, to_upper_char
*        added sentence_of
* - 8.25 ok - provides prolog files from prolog.jar - when serving http
* - 8.23 fixed bug in HTTP server - should skip unknown header elements
* - 8.20 added ability to change fonts to GUI
* - 8.20 fixed bug with GUI console hanging because of dead engines
* - 8.18 added ucompile/1 and /3 supporting conversion to
*        bytecode of *.pl user projects and creation of 
*        standalone bytecode user applications
* - 8.15 changed ZIPSTORE to prolog.jar
*        added lightweight HTTP server
* - 8.15 fixed bug with new_console - GUI - sread was failing
*        making the console hang
*        related: also revisited topcall vs. wrapped_call
*        now GUI catches it's own exceptions
* - 8.10 fixed bug with single quote - toQuoted should give ''''
* - 8.05 fixed bug with class fields not being visible in embedded instances
*        by making sure that class fields always refer to database 0
*        note that this_db referes to the current (new) instance each time
*        even if the <class>:action notation is used
* 
* - 8.02 bytecode to java translator creates separate files now
* 
* - 8.01 fixed bug with hashkey based encoding
* 
* - 7.97 added simple XOR encryption to the resulting compressed classes
*        this provides to owners of source licenses to protect the code
*        of their binary knowledge bases
* - 7.96 added zip/unzip to make javafied code more compact: code is now
*        encapsulated in an array of shorts
* 
* - 7.82 added Javafier - converts bytecode in wam.bp to
*        small java classes - meant to make deployment of
*        Jinni - which now becomes simply a set of Java classes
*        with no other data - eaisier
*        java -jar prolog.jar
*        is now a convenient way to run such components
* - 7.70 made swrite fully canonical - added canonicalTermToString
*        ensures socket interoperation with virtually any Prolog
*        regardles of presence or absence of identical operator definitions
*        terms over sockets are now written in prefix syntax - except for lists
* 
* - 7.66 extended reflection interface to be able to name
*        one of the (multiple) classes or interfaces and object
*        needs to be seen as implementing
* 
*        added to classlib interface.pl and enumeration.pl
*        to support use of interfaces through reflection
* 
*        extended vector.pl and hashtable.pl to support
*        enumerations
* 
* - 7.65 added vector and hashtable to and from list conversions
*        extended classlib wrappers for hashtable and vector
*        added HeapRef class - to allow to pass back data they
*        build on the heap from methods of Machine called through reflection
*        => this makes Jinni extensible with very fast builtins
*
*  - 7.64 modified ide to include file_dialogs to load and save files
*    unfortunately file filters do not work on Windows
*      
* - 7.6x added XML interface as separate module
* 
* - 7.60 fixed typo in run_server
* 
* - 7.5x - added $encoded(T) and '$portable(T) notation forcing interface to
*        pass term as encoded/decoded copy and portable copies
* 
*  -  added Portable Terms - encoded + symbol table
*     ready to efficiently represent terms in serialized form
* 
*  -  7.57 made encoded terms lighter - ready for
*     using them for sending terms over the net,
*     persistence, etc.
* 
*  -  7.56 - fixed term database operations
*  
*  -  7.55 - starting experiments for using engines as fast dynamic databases
* 
*  -  7.54 - added to_engine/2 and from_engine/1 to
*     pass terms efficiently between engines
* 
*  -  7.52 - sent out as free update on May 16, 2002
* 
*  -  7.52 passed bm with external db representation
* 
*  -  7.51 fixed bug with double reconsult - uses internal dif list db repr
*
*   - 7.50 added support for Assumption Grammars - all at source level
*          on top of classic DCG preporocessor 
*     - added assumptions.pl and progs/ag_parser example - basic AG based Prolog parser
*     - modified DCG proprocessor to expand # to #: 
*   - 7.48 - replaced fullDeref with FDEREF()
*   - 7.45 added add_undoable/1 - allowing use of Jinni as a generic backtracking engine
*          acting upon Java objects implementing the interface Undoable (undo()+done())
*          updated UserGuide
* 
*   - 7.44 added class variable fields with v<==a and v==>X operations
* 
*   - 7.42 added backtrackable field assignment uget, uput
* 
*   - 7.41 fixed problems with tags - all tags (except VARs) commute
*          and tag 11 is now available for use in constraint processing
*          or something similar - as a marker for $object handles - which
*          could as a result be collected on backtracking by having their
*          undo() method called when the trail is scanned
*          note that this could be as well achived by using symbols
*          it might make some sense to use tag 11 for symbolic constants -
*          this will allow having up to 2^30 - but it would require quite
*          a few changes
* 
*   - 7.38 fixed bug in match_term - assuming >0 is the same as !=0 =>
*          now FUNTAG and INTTAG commute
* 
*   - 7.36 fixed Machine.termToString bug
*   
* * - 7.33 consult bug fixed !!!
*   - bug from 5.xx->7.33: 
*        -- 7.06??? introduces random LOSS of clauses on consult/1
*           => in fact it is an OLD bug: it shows up if consult is used
*              BEFORE a compile - coming from the Dict.rehash0() function
* 
*  7.31 - debugging - about to switch to new O(1) db operations
* 
*  7.29-35 - bug in Machine.termToString - returning null is wrong
* 
*  7.27 - refactoring finished - new all in one prolog.kernel package
*         build -> bin directory sytructure
*         *.bat files replaced by all-in-one build.pl BinProlog system script 
* 
*  7.15 - start refactoring of Jinni package structure
* 
*  7.13 - some refactoring - oved ThinJinni and JavaCallsJinni to xjinni2k
* 
*  7.12 - fixed bug with serialization - comung from not 
*         clearing Var tables after conversion
* 
*  7.06 - added support for clause(X,Y) with X unbound
*         and listing/0 - INTRODUCES bug: consult might lose clauses
* 
*  7.05 - added cryptography and external serializtion of terms
*       - added new client and server classes for high volume
*         RPC transaction - which reuse sockets
*       - added pp_clause for printing out terms
*       - reversed printing out Java objects through their to String
*         methods - it is now done in a fully reversible way - by
*         keeping them in the for '$object'(int_handle) When read back,
*         it is still the programmer's resonsbility that they are
*         meaningful - but this allows them to be sent over sockets and
*         back - if needed. Serialized terms can be used if Java objects
*         need to be sent over the network - Jinni terms will embed them
*         and the cryptographic module will hide them in Sealed Terms.
*   
*  6.86 - added some friendly constructors to Fun:
*         Fun F=new Fun("f",new Integer(9),"hello")
*         will work now.
* 
*  6.84 - added support for passing compound terms and
*         handles to Java Objects - on which Jinni can
*         use callback methods
* 
*  6.82  -changed termReader.putObject to build Prolog terms from Fun, Var and
*        basic Java objects (Strings, Integers, Floats)
*        -putResult in Extender is now just a call to putObject
*  6.78  added Fun+Var and modified Extender to support arbirary
*        Prolog terms to be stored in Java data structures
*        as trees built of Fun, var and natural mappings
*        of contants to Strings, Integers, Doubles
* 
*  6.78  added wrapper for Object - and modified Vector and ObjectDict
*        to extend the java_object wrapper
*        Prolog objects could extend Java objects via Reflection - and
*        a nice way to build wrappers is to inherit - at Prolog level
*        from what the wrapped Java objects inherit from 
* 
*  6.73  constructors are inherited !!! - also fixed small file-file.pl confusion bug
* 
*   it seems cool after all to allow inheriting N-arg constructors
*   we will proceed to support it - it probably did not make it into some OO
*   languages because it is somewhat tricky to implement - but we are in Prolog!
*         
*   - key questions: (see UserGuide for our answers) 
*        -- only noarg constructors are called?
*        -- can we inherit the closest matching arity super's constructor?
*        -- Prolog objects could extend Java objects via Reflection - but 
*           should they?
* 
*  6.72  released/announced as Jinni 2002
* 
*  6.69  completely cleaned up interpreter related stubs
* 
*  6.68: moved ":" (xfy)  and =>,<= just bellow "," and ";" priority
*        added stacks,lists,queues to classlib
* 
* 
*  6.67: handled Java stack oweflows from printing or copying
*        circular terms - see Main.RECURSION_DEPTH
*        this make Jinni robust even if the Java runtime (i.e. jview)
*        cannot always detect it's own stack overflow
* 
*  6.62 committing to OO model:
* 
*   - multiple (possible cyclic inheritance)
*   - no-arg constructors of supers automatically called
*   - DF visit order of includes - with elimination of duplications
*     (also handles cycle detection)
*   - programmers should define most field's default constructors
*     => and <= freed for get/set operations on fields
*   - to keep it simple, no mess with public/private - security
*     will be provided through different means
* 
* - 6.59 - added Brad's Extender.java - handling the special
*          case of the class Class' own methods
* - 6.57 - fixed bug in pushList => inducing name/2 problems
*        - expected to solve some other problems
* - 6.52 - added file_dialogs a new_file_editor
*   - we write quoted terms - for general
*     read/write reversibility
*   - added trim_quotes - in Prolog
* 
*  -6.47: fixed bug with getHome() - now all filtered through JavaIO.getHome()
*         http://.. and C:\.. are recognized as "absolute"
*   also fixed bug with read() which nnow returns - as it should - end_of _file
* 
* - <6.44 - modified qcompile,mcompile etc. to support
*   prolog based scripting for creating Prolog classes and
*   clause server (debug) classes
* - Prolog classes can now be built/discarded dynamically
*   Class:Notation - roughly like a call to a Prolog module
* 
* - 6.34 - made tetris demo stop in a clean way - added version interface
* - 6.30 - fixed bm - to work as a regression test
* - 6.29 - fairly stable - could be used for new release
* - 6.29 - fixed bug - cloned Prolog was not yet
*   in Atom table when calling Machine - calling reflection
*   at that point failed
* 
* - 6.28 - simplified - removed commit
* - 6.23 - started to fix tranlate_all_files
*   => supports multiple command line [file] commands
* 
* - 6.22 - added module:Goal notation - it works!!!
* 
* - 6.22 fixed rollback related bug - see bm.pl->bug1
* 
* - 6.20 split jlib.pl - fixed enter/2, bug in CodeStore cloning
* - 6.15 adjusted PocketPC sizes - tested with ide, tetris, vq8
* - 6.13 - got applets to work - in new frame
* - most GUI ported to new all in Prolog design
* - ide,console,editor work
* 
* changed Buttons - and dialog - with Hubs, and internal terms
* 
* REMOVED Interpreter - moved to xjinni2k 
* Reflection based GUI runs tetris and queens
* 
* TODO: work on porting GUI library
* 
* started to move GUI to compiled jinni + reflection
* 
* fixed bug with socket overflow - now both service and
* client close sockets - still - to consider 
* transparent socket caching
* 
* WORKS ON IPAQ 10-15K LIPS !!!
* 
* trying out Personal Java compatibility - hoping to have it run on IPAQ
* 
* added waiting in Transport.create_socket - broken Windows OS
* does not close sockets properly - and runs out of them under
* heavy load
* 
* fixed bug/feature in findall - when result list was bound to []
* it did not run all alternatives
* 
* fixed floating point representation and comparison bugs
* => runs ffibo
* replaced threads + Linda operations with compiled+reflection code
* moved remote_run, bg, in, out etc. to jlib.pl
* 
* Beta release of reflection package
* 
* - added field handling to reflection API
* - added reflection API to jlib.pl
* - constructors and method invocation work with ARGS !!!
* - finished new_java_class reflection based builtin
* - enabled Extender with new Converter - later in TermReader
* - added parameter conversion through ITerm and OTerm interfaces
* 
* start to add reflection based builtins
* to BuiltinIDs.java, Machine.java, co.pl
* 
* skiped possibly lost versions
* 
* changed distribution to make sure applets work on old browsers
* 
* fixed interpreter toplevel bug
* 
* got Visual Tetris demo work - fixed bug - now stops when full
* 
* optimized remote_run > 100 exchanges / sec
* 
* figured out relative slowness of Transport - it seems
* to come from slow compiler to interpreter calls
* 
 NO BUG with cmd line params: mjinni "println([aa,bb,cc])" - in fact is just Windows ...

 FIXED  
 
 - in/out memory leak => move Threads and BBoards to compiler
 
 - done: further improve the dynamic database by
   using direct IntQueues attached to extensive
   indexing front-end
 
 - done - but interp removed - 
     make Interpreter extensions like remote_run - to actually refer to compiled code
 
 - done: interp removed: get rid of interpreted execution as much as possible
<pre>
*/

public class Main {
  // see versions in Init
  public static void main(String[] argv) {
    //Top.testProlog();
    /*
    System.err.println("ARGS:\n"+argv.length);
    for(int i=0;i<argv.length;i++) {
      System.err.println("argv:"+i+"="+argv[i]+"\n");
    }
    */
    //Top.jinniMain(argv);
    // params: cmd line args,stdin,stdout,prompt
    (new Shell(argv,null,null,true)).run();
  }
}

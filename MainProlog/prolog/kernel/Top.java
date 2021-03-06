package prolog.kernel;
import prolog.logic.*;
import java.io.ByteArrayOutputStream;
/**
  De facto Main class - used by Main - which is kept lightweight.
  Called though Reflection from Prolog - and from outside modules.
*/
public final class Top {

  
  
   /**
   *  this class embeds a Machine into your application
   */
  
  public static Machine newProlog(String bpFile) {
     // bpFile becomes default embedded wam.bp if null
    
     Machine I=new_machine();
     //I.query_engine(new Fun("ttyprint","boo"));
     //I.load_engine(new Fun(":-","done","$prolog_loop"));
     return I;
  }
  
  /*
  public static Object[] askProlog(Machine I,Object[] bundle) {
     I.setBundle(bundle);
     Prolog.dump("PTERM:"+new PortableTerm(bundle,0));
     Prolog.dump("askProlog GOT TERM:"+I.fromBundle(bundle));
     try {
       int t=I.ask();
       if(0==t) return null;
     }
     catch(Exception e) {
        e.printStackTrace();
        return null;
     }
     return I.getBundle();
  }
  */
  
  public static Object[] askProlog(Machine I,Object[] bundle) {
     //if(null==bundle) return null;
     //Prolog.dump("PTERM:"+new PortableTerm(bundle,0));
     
     //Prolog.dump("askProlog GOT TERM:"+I.fromBundle(bundle));
    
     PortableTerm P=new PortableTerm(bundle,0);
     //Machine M=new_machine();
     EncodedTerm T=P.outputTo(I.prolog);
     //M.stop();
     I.load_engine(T);
     int t=0;
     try {
       t=I.ask();
       if(0==t) return null;
     }
     catch(Exception e) {
        e.printStackTrace();
        return null;
     }
     T=I.encodedCopy(t);
     P=new PortableTerm(T,I.prolog);
     bundle=P.export(0);
     
     //Prolog.dump("askProlog RETURNS TERM:"+I.fromBundle(bundle));
     
     return bundle;
  }
  
  //EncodedTerm T=P.outputTo(prolog);
  
  public static void stopProlog(Machine I) {
     I.stop();
  }
  
  
  // API tester
  
   public static void testProlog() {
     Fun goal=new Fun("println","hello");
     Machine C=new_machine();
     Object[] bundle=C.toBundle(goal);
     if(null==bundle) return;
     Machine I=newProlog(null);
     bundle=askProlog(I,bundle);
     if(null==bundle) return;
     Prolog.dump("ANSWER BUNDLE:"+new PortableTerm(bundle,1));
     Prolog.dump("ANSWER TERM:"+C.fromBundle(bundle));
   }

  
  
  
  
  public static int get_verbosity() {
    return Interact.verbosity;
  }
  
  public static void set_verbosity(int level) {
    Interact.verbosity=level;
  }
  
  public static int get_quickfail() {
    return Interact.quickfail;
  }
  
  public static void set_quickfail(int level) {
    Interact.quickfail=level;
  }
  
  public static String defaultEncoding="UTF-8"; // same as "UTF8"

  public static void setEncoding(String encoding) {
    defaultEncoding=encoding;
  }

  public static String getEncoding() {
    return defaultEncoding;
  }
  
  /** default jar or zip file containing all our Java classes 
   * and Prolog bytecode
   **/
  public static String ZIPSTORE="prolog.jar";

  /** default jar or zip file containing all our Java classes 
   *  and Prolog bytecode
   **/

  public static String JINNI_HOME="";


  public static final String getPrologName() {
    return Init.getPrologName();
  }

  public static final int getPrologVersion() {
    return Init.XBRAND_MINOR;
  }

  private static ObjectDict dict=new ObjectDict();

  public synchronized static final Object getProp(Object key) {
    return dict.get(key);
  }

  public synchronized static final void setProp(Object key,Object val) {
    dict.put(key,val);
  }

  public synchronized static final void rmProp(Object key) {
    dict.remove(key);
  }

  public synchronized static final void saveProps(String file) {
    Extender.toFile(file,dict);
  }

  public synchronized static final void loadProps(String file) {
    dict=(ObjectDict)Extender.fromFile(file);
  }

  public synchronized static final void clearProps() {
    dict=new ObjectDict();
  }


  /**
  Initializes the runtime system and returns a new Machine
  ready to run queries. 

  args[0] contains the absolute path to
  the *.zip file (usually prolog.zip) containing our java
  classes (usually prolog.kernel.*) and Prolog 
  bytecode (usually in wam.bp).

  args[k] with k>0 contain Prolog goals to be run in
  a default Prolog envoronment before control is given
  to an interactive shell (the Prolog toplevel).

  The second argument, is a boolean requesting, if true
  suppression of various Prolog messages.

  */

  final public static Machine initProlog(String[] argv) {
    return initProlog(argv,null,null);
  }

  final public static Machine initProlog(String[] argv,PrologReader input,PrologWriter output) {
    Init.greeting(get_verbosity());
    int argc=0;

    //if(null!=argv) for(int j=0;j<argv.length;j++) {Prolog.dump("argv["+j+"]="+argv[j]);}

    if (null==argv) {
      // nothing to do - defaults are fine
    }
    else if (argv.length>0&&argv[0].indexOf('(')>=0&&argv[0].indexOf(')')>0) {
      // nothing to do - this is not a file or url command !!!
    }
    else if (argv.length>0&&argv[0].endsWith(".bp")) {
      Interact.PROLOG_BYTECODE_FILE=argv[0];
      Extender.turnOff(); // overrides loading of Javafied default bytecode
      argc++;
      //Prolog.dump("found=>"+argv[0]);
    }
    else if (argv.length>0&&argv[0].endsWith(".jc")) {
      Extender.turnOff(); // overrides loading of Javafied default bytecode
      argc++;
      //Machine M=new_machine(argv[0]);Prolog.setDefaultProlog(M.prolog);return M;
      Prolog.setDefaultProlog(new_prolog(argv[0]));
    }
    else if (argv.length>0&&(
              argv[0].equals("..")||argv[0].equals(".")||
              argv[0].indexOf('/')>=0||argv[0].indexOf('\\')>=0
           )) {
      if (argv[0].endsWith(".zip")||argv[0].endsWith(".jar")) {
        ZIPSTORE=argv[0];
        JINNI_HOME=JavaIO.pathOf(argv[0]);
      }
      else {
        char last=argv[0].charAt(argv[0].length()-1);
        if ('/'==last||'\\'==last)
          JINNI_HOME=argv[0];
        else
          JINNI_HOME=argv[0]+'/';
      }
      argc++;
    }
    else if (argv.length>0&&(argv[0].endsWith(".zip")||argv[0].endsWith(".jar"))) {
      ZIPSTORE=argv[0];
      argc++;
    }

    Machine machine=new_machine(input,output);

    if (null==machine) return null;
    //if(null==argv||0==argv.length) return machine;

    if (Init.XBRAND_START_IDE>0) {
      if (null==argv||0==argv.length) {
        argv=new String[2]; argv[0]="ide"; argv[1]="sleep(500000)"; argc=0;
      }
    }
    // splashes away with this !!!
    //if(null==argv||0==argv.length) {argv=new String[1]; argc=0; argv[0]="ide";}


    boolean gotInitFile=false;
    for (int i=argc;i<argv.length;i++) {
      String query=argv[i];

      if (!gotInitFile&&(query.endsWith(".pl")||query.endsWith(".pro"))||query.endsWith(".jpl")||query.endsWith("]")) {
        gotInitFile=true; // the compiler can only handle one initial file at this point...
        if (!query.endsWith("]"))
          query="'"+query+"'";
        query="compile("+query+")";
      }
      //JavaIO.println("cmd query ?- "+ query+".");
      String answer=machine.run(query);
      //JavaIO.println("cmd answer => "+ answer);
      if (answer.equals("no")||answer.startsWith("the(exception(")) {
        JavaIO.warnmes("error trying: "+query);
        machine.stop();
        machine=null;
        break;
      }
    }
    return machine;
  }

  /**
 * Starts a new Machine and Prolog toplevel
* - deprecated -
*/

  public static void toplevel(Machine machine) {
    long duration;
    String answer;
    PrologReader input=null;
    PrologWriter output=null;

    for (;;) {
      duration=System.currentTimeMillis();
      answer=machine.run(null);
      JavaIO.println("Toplevel engine returned: "+answer);

      if (!answer.equals("no")) break;

      machine=new_machine(input,output);
      if (null==machine) break; // regenerate machine
      duration=System.currentTimeMillis()-duration;

      if (duration<400L) break;
    }
    JavaIO.halt(0);
  }


  public static void toplevel() {
    (new Shell(null,null,null,false)).run();
  }

  /**
   * Returns a basic LogicEngine - with basic Prolog functionality
   * but no io, reflection etc.
   */


  /**
  *  Returns a new Machine - de facto constructor to be used from
  *  outside the package
  */
  public final static Machine new_machine(Prolog P,PrologReader input,PrologWriter output) {
    return Machine.new_machine(P,input,output);
  }

  public final static Machine new_machine(Prolog P) {
    if(null==P) P=Prolog.getDefaultProlog();
    return Machine.new_machine(P,null,null);
  }

  public final static Machine new_machine(PrologReader input,PrologWriter output) {
    return new_machine(Prolog.getDefaultProlog(),input,output);
  }

  public final static Machine new_machine() {
    return new_machine(null,null);
  }

  public final static Machine new_machine(String serializedFname) {
    Prolog P=new_prolog(serializedFname);
    Prolog.setDefaultProlog(P);
    return new_machine(null,null);
  }

  public final static String collect_call(Object query) {
    ByteArrayOutputStream output=new ByteArrayOutputStream();
    PrologWriter stringOutput=new PrologWriter(output);
    Fun G=new Fun("and",query,"fail");
    Machine E=Top.new_machine(null,stringOutput);
    E.query_engine(G);
    return output.toString();
  }

  public final static Object call(Object query) {
    Machine E=Top.new_machine();
    Object answer=E.query_engine(query);
    E.stop();
    return answer;
  }

  public final static Machine new_cached_machine(String serializedClassName,
      PrologReader R,PrologWriter W) {
    Prolog P=Prolog.getPrologClass(serializedClassName);
    if (null==P) {
      P=new_prolog(serializedClassName+".jc");
      if (null==P) return null;
      Prolog.addPrologClass(serializedClassName,P);
    }
    return new_machine(P,R,W);
  }

  public final static Machine new_cached_machine(String serializedClassName) {
    return new_cached_machine(serializedClassName,null,null);
  }

  public final static Prolog new_prolog(String serializedFname) {
    Prolog prolog=(Prolog)Extender.fromFile(serializedFname);
    prolog.refresh();
    return prolog;
  }

  /** 
 * Main entry point. Command line parameters include the Prolog home
 * directory (or zip or jar file) the name of the *.bp bytecode file as well as
 * Prolog goals to be run before the toplevel interpreter (with the ?- prompt)
 * is started. No need for heap,stack etc. related parameters, all memory managment
 * is automated.
*/
  public static void jinniMain(String[] argv) {
  
    try {
      Machine M=initProlog(argv); // quietness: false means verbose
      if (M!=null) {
        toplevel(M);
      }
    }
    catch (Throwable e) {
      JavaIO.errmes("irrecoverable "+getPrologName()+" error",e);
    }
  }


}


package prolog.kernel;
import prolog.logic.*;
import java.io.*;

/**
   Interactive Console Shell Abstraction For Querying the 
   Prolog Machine. Designed to be overridden by Remote, Applet
   or Web based shells or shells for machines without a
   console like Pocket PC. It can be seen as a "skin" for
   the Prolog interpreter.
 */

public class Shell implements Runnable, Stateful {

  public Shell(String args[], PrologReader input, PrologWriter output,
   boolean init, String prompt) {
    this.input = input;
    this.output = output;
    if (init) this.M = Top.initProlog(args, input, output);
    else restart_machine(true);
    this.prompt = prompt;
    this.init = init;
    //System.err.println("mclass="+M.getClass());
  }

  private Machine M;
  private String prompt;
  private boolean init;
  private PrologReader input;
  private PrologWriter output;

  public void restart_machine(boolean ok) {
    this.M = Top.new_machine(this.input, this.output);
    if (ok) JavaIO.warnmes("Starting New Prolog Machine.");
    else JavaIO.warnmes("Restarting Prolog Machine: " + M);
  }

  public Shell(String args[], PrologReader input, PrologWriter output, boolean init) {
    this(args, input, output, init, "?- ");
  }

  public Shell(String args[], PrologReader input, PrologWriter output) {
    this(args, input, output, true);
  }

  public Shell(String[] args) {
    this(args, null, null);
  }

  public Shell() {
    this(null);
  }

  // begin IO
  public String readln() {
    //return M.readln();
    if(null==input) return consoleReadln();
    return JavaIO.readln();
    
  }

  public void print(String S) {
    //M.print(S);
    JavaIO.print(S);
  }

  public void println(String S) {
    //M.println(S);
    JavaIO.println(S);
  }
  // end IO

  public String readQuery() {
    M.print(prompt); M.flush();
    if(null==input) return consoleReadln();
    return M.readln();
  }

  public void send_query(String S) {
    M.load_engine(make_query(S));
  }

  public Object make_query(String S) {
    //String SGoal="__A:-scall('("+S+")',__A)";  

    Var Answer = new Var(1); //only var!
    Fun SGoal = new Fun(":-",
      Answer,
      new Fun("scall", S, Answer)
    );

    return SGoal;
  }

  public String get_answer_element() throws PrologException {
    int v = M.ask();
    if (0 == v) return null;
    String S = M.termToString(v);
    S = M.unQuote(S);
    return S;
  }


  public boolean shell_step(String S) throws PrologException {
    send_query(S);

    for (int i = 0; ; i++) {
      String A = get_answer_element();
      if (null == A) break;
      if ("no".equals(A) || "yes".equals(A)) {
        if (i > 0) println(";");
        println(A); println("");
        break;
      }
      else if ("new".equals(A)) {
        if (i > 0) println(";");
      }
      else if ("=".equals(A)) {
        String X = get_answer_element();
        String T = get_answer_element();
        println(X + " = " + T);
      }
      else {
        //println("unexpected from engine: "+A+" at step: "+i);
        println(M + " returned:: " + A);
        return false;
      }
    }
    return true;
  }

  public void run() {
    if (null == M) {
      JavaIO.warnmes("unable to initialize Prolog engine");
      return;
    }
    for (; ; ) {
      String S = readQuery();
      if (null == S) break;
      if ("".equals(S)) continue;

      boolean ok;
      try {
        ok = shell_step(S);
      }
      catch (PrologException e) {
        //JavaIO.errmes("Error caught by Shell: ", e);
        Interact.warnmes("Error caught by Shell!");
        ok = false;
      }
      if (!ok) restart_machine(false);
    }
    if (init) JavaIO.halt(0);
  }
  
  
  
  
  // using jline
  static final private jline.ConsoleReader in=makeReader();
  
  private static jline.ConsoleReader makeReader() {
    try {
      return new jline.ConsoleReader();
    } catch(Exception e) {
      Interact.errmes("unable to start ConsoleIO");
      return null;
    }
  }
  
  public String consoleReadln() {
    try {
      return in.readLine("");
    }
    catch(Exception e) {
      e.printStackTrace();
      return "";
    }
  }
  
  public int read() throws IOException {
    return in.readVirtualKey();
  }
  
}
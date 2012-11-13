package prolog.logic;
import java.io.*;

/**
 * Proposed interaction model:
 * 
 * send(channel/agent,Term,Timeout)
 * receive(channel/agent,Term,Timout)
 */
public class Interact {

  public static int verbosity=2;

  public static int quickfail = 1; // the higher the less fault tolerant we are
  public static String PROLOG_BYTECODE_FILE = "wam.bp";

  public static boolean isApplet = false;
  public static char XNL = '\n';// see also PrologReader.NL which is always '\n'
  public static String NL = System.getProperty("line.separator");  // has length=2 on Windows!!!

  private static final String[] default_user_path ={ "", "agentlib/", "classlib/", "vprogs/", "progs/", "../psrc/" };

  public static final String[] applet_user_path = default_user_path;


  private static final String fix_dir(String s) {
    if ("".equals(s) || s.endsWith("/") || s.endsWith("\\")) return s;
    return s + "/";
  }

  /**
   *  adds a new directory or URL to the end of Prolog's search path
   */
  public static void add_to_path(String dir) {
    dir = fix_dir(dir);
    if (USER_PATH.contains(dir) == -1) USER_PATH.enq(dir);
  }


  /**
   *  adds a new directory or URL to the beginning of Prolog's search path
   */
  public static void push_to_path(String dir) {
    dir = fix_dir(dir);
    if (USER_PATH.contains(dir) != -1) USER_PATH.delq(dir);
    USER_PATH.pushq(dir);
  }

  /**
   *  deletes a directory or URLfrom Prolog's search path
   */
  public static void del_from_path(String dir) {
    dir = fix_dir(dir);
    //USER_PATH.removeElement(dir);
    USER_PATH.delq(dir);
  }

  /**
   *  returns the i-th directory on Prolog's search path
   */
  public static String path_element(int i) {
    if (i < 0 || i >= USER_PATH.size()) return null;
    return (String)USER_PATH.elementAt(i);
  }

  /**
   *  completely clears the Prolog search path
   */
  public static void clear_path() {
    //USER_PATH.removeAllElements();
    USER_PATH.clear();
  }

  public static ObjectQueue USER_PATH = new ObjectQueue(default_user_path);

  public static void println(Object O) {
    System.out.println(O.toString());
  }
  
  public static final void warnmes(String s) {
    if (verbosity>=2) println(s);
  }
  
  public static final void warnmes(Throwable e) {
    warnmes(e.toString());
    if(quickfail>=2 || verbosity>=3) printStackTrace(e);
    if (quickfail>=3) {
      println("HALTING, quickfail MODE");
      halt(99);
    }
  }

  public static void printStackTrace(Throwable e) {
    if (verbosity>=1) {
      //CharArrayWriter b=new CharArrayWriter();
      ByteArrayOutputStream b = new ByteArrayOutputStream();
      PrintWriter fb = new PrintWriter(b); // was PrologWriter - for case of IDE redirection
      e.printStackTrace(fb); fb.flush();
      warnmes("/*\n" + b.toString() + "\n*/");
      fb.close();
    }
  }

  synchronized public static final void errmes(String s, Throwable e) {
    warnmes(s);
    if (verbosity>=1 && null != e) {
      warnmes(e);
    }
  }

  synchronized public static final void errmes(String Mes) {
    errmes("??? " + Mes, (new Exception("error locator")));
  }

  synchronized public static final void fatal_error(String Mes) {
    quickfail=10;
    errmes("!!! " + Mes, (new Exception("error locator")));
  }

  static long time = System.currentTimeMillis();

  static void endMes(int code) {
    time = System.currentTimeMillis() - time;
    String scode=(0==code)?"":"("+code+")";
    println("Prolog execution halted"+scode+". CPU time = " + (time / 1000.0));
    println("expand/shrink time=" + HeapStack.mmTIME + " GC time:" + PrologGC.gcTIME);
  }

  public static final void halt(String mes) {
    errmes("FATAL ERROR:", (new java.lang.Exception(mes)));
    halt(1);
  }

  public static final void halt(int code) {
    if (!isApplet) shutdown(code);
    else println("Exit code " + code + " ignored!");
    //else applet.destroy();
  }

  public static final void shutdown(int code) {
    endMes(code);
    System.exit(code);
  }

  public static int RECURSION_DEPTH = 2048; // avoids Java stack overflows
}

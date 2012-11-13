package prolog.kernel;
import prolog.logic.*;

import java.io.*;
import java.text.NumberFormat;

/**
* Instance of the main logic engine
*/
final public class Machine extends LogicEngine {

  /**
  Builds a new Prolog Machine and initialize.

  String representations of answers can be produced
  by calling run(), which returns null when
  no more ansers can be produced.
 
  The Machine can be halted by calling stop().
  */
 
 final static Machine new_machine(Prolog P,PrologReader input,PrologWriter output) {
   if(null==P) return null;
   Machine M=new Machine(P,input,output);
   return M;
 }
 
 public Machine to_runnable(Object query) {
   Machine M=newMachine(prolog);
   if(!M.load_engine(query)) return null;
   return M;
 }
 
 
 final static Machine new_machine(PrologReader input,PrologWriter output) {
   Prolog P=Prolog.getDefaultProlog();
   return new_machine(P,input,output);
 }
 
  public final Object getTerm(int xref) throws PrologException {
    return this.termReader.getTerm(xref);
  }

  final public Object get_answer() {
    Object answer;
    try {
      int result = ask();
      if (0 == result) answer = null;
      else answer = termReader.getTerm(result);
    }
    catch (Exception e) {
      JavaIO.errmes(this + ":error:", e);
      answer = "exception('" + e + "')";
    }
    return answer;
  }


  final public String ask_string() throws PrologException {
    int t = ask();
    if (0 == t) return null;
    else return termToString(t);
  }

  final public String run(String query) {
    String answer = null;
    try {
      load_engine(query);
      answer = ask_string();
    }
    catch (Exception e) {
      JavaIO.errmes("error in machine.run()", e);
      answer = "exception('" + e + "')";
    }
    if (null == answer) answer = "no";
    else answer = "the(" + answer + ")";
    return answer;
  }

 public Object[] toBundle(Object fun) {
    Fun query=new Fun(":-","ok",new Fun("export_term",fun));
    Object[] bundle=null;
    try {
      if(load_engine(query) && 0!=ask()) {
        bundle=getBundle();
        setBundle(null);
      }
    } catch(Exception e) {e.printStackTrace();}
    return bundle;
  }
  
  
  public Object fromBundle(Object[] bundle) {
    setBundle(bundle);
    Var X=new Var(0);
    Fun query=new Fun(":-",X,new Fun("import_term",X));
    Fun theAnswer=(Fun)query_engine(query);
    setBundle(null);
    return theAnswer.getArg(1);
  }

 
  /**
   * Creates a new engine with given input and output. As Engines create other engines
   * a static pseudo-constructor is needed to ensure that upon "cloning" they create 
   * another engine of the their own kind. See also the class Machine.
   */
  Machine newMachine(Prolog prolog, PrologReader input, PrologWriter output) {
    //Prolog.dump("creating a Machine, input="+input+",output="+output);
    return new Machine(prolog, input, output);
  }

  public LogicEngine newLogicEngine(Prolog prolog) {
    return newMachine(prolog);
  }

  Machine newMachine(Prolog prolog) {
    //Prolog.dump("creating a Machine");
    return newMachine(prolog, this.input, this.output);
  }

  Machine(Prolog prolog, PrologReader input, PrologWriter output) {
    super(prolog);

    this.input = def_input(input);
    this.output = def_output(output);

    init_see_tell();

    this.extender = new Extender(this);
  }

  // would require removing final attribut for input,output 
  public void set_input(PrologReader input) {
    this.input=def_input(input);
    init_see_tell();
  }


  public void set_output(PrologWriter output) {
    this.output=def_output(output);
    init_see_tell();
  }

  public TextSink getTextSink() {
    return this.output.getTextSink();
  }
  
   public final void destroy() {
    super.destroy();
    destroy_io();
  }

  PrologReader def_input(PrologReader input) {
    return (null != input) ? input : JavaIO.getStdInput();
  }

  PrologWriter def_output(PrologWriter output) {
    return (null != output) ? output : JavaIO.getStdOutput();
  }



  public String toString() {
    return getClass().getName() + " " + get_class_name() + " " + get_instance_id();
  }



  /*
   * Machine State Variables 
   */
  protected  PrologReader input;
  protected  PrologWriter output;
  private Extender extender;


  private ObjectDict G_seeFiles;
  private ObjectDict G_tellFiles;

  private int G_seefunc;
  private int G_tellfunc;

  private PrologReader G_seefile;
  private PrologWriter G_tellfile;

  void init_see_tell() {
    //Prolog.dump("init_see_tell:prolog:"+prolog);
    G_seefunc = prolog.G_user;
    G_tellfunc = prolog.G_user;
    G_seefile = input;
    G_tellfile = output;
    G_seeFiles = new ObjectDict();
    G_tellFiles = new ObjectDict();
    G_seeFiles.put(new Integer(prolog.G_user), G_seefile);
    G_tellFiles.put(new Integer(prolog.G_user), G_tellfile);
  }

  protected void destroy_io() {
    G_seeFiles = null;
    G_tellFiles = null;
  }

  public String readln() {
    return G_seefile.readln();
  }

  public void print(String s) {
    //System.err.println(JavaIO.NL+"{"+output+"=i<>t="+G_tellfile+"}print: => <"+s+">");
    G_tellfile.print(s);
  }

  public void nl() {
    println("");
  }

  public void println(String s) {
    G_tellfile.println(s);
  }

  //public String termToString(int t) {return super.termToString(t);}

  public static String unQuote(String s) {
    final char quote = '\'';
    int l = s.length();
    if (l > 1 && s.charAt(0) == quote && s.charAt(l - 1) == quote) {
      s = s.substring(1, l - 1);
    }
    return s;
  }

  public void clear() {
    super.clear();
  }

  public void flush() {
    G_tellfile.flush();
  }

  private boolean tell(int xval) throws PrologException {
    if (isVAR(xval) || isINTEGER(xval)) {
      warn_mes(xval + " bad file name in TELL");
      return false;
    }

    PrologWriter tell = (PrologWriter)G_tellFiles.get(new Integer(xval));
    if (tell == null) {
      String fileName = getAtomName(xval);
      try {
        tell = new PrologWriter(fileName);
        Integer func = new Integer(prolog.atomTable.newFunctor(fileName, 0));
        G_tellFiles.put(func, tell);
      }
      catch (IOException e) {
        JavaIO.errmes("error trying to write to file: " + fileName, e);
        return false;
      }
    }
    /* already opened is ok */
    G_tellfile = tell;
    G_tellfunc = xval;
    return true;
  }

  private boolean see(int xval) {
    if (isVAR(xval) || isINTEGER(xval)) {
      warn_mes(xval + " bad file name in SEE");
      return false;
    }

    PrologReader see = (PrologReader)G_seeFiles.get(new Integer(xval));
    if (see == null) {
      String fileName = getAtomName(xval);
      try {
        see = JavaIO.toReader(fileName);
        Integer func = new Integer(prolog.atomTable.newFunctor(fileName, 0));
        G_seeFiles.put(func,see);
      }
      catch (PrologException e) {
        JavaIO.traceln("error trying to read from file: " + fileName);
      }
      if (null == see) return false;
    }
    /* already opened is ok */
    G_seefile = see;
    G_seefunc = xval;
    return true;
  }

  public int getLineNumber() {
    return G_seefile.getLineNumber();
  }

  public void setLineNumber(int i) {
    G_seefile.setLineNumber(i);
  }

  private void told() {
    if (G_tellfile != output) {
      Integer tellFunc = new Integer(G_tellfunc);
      PrologWriter file = (PrologWriter)G_tellFiles.get(tellFunc);
      if (file != null) {
        file.close();
        G_tellFiles.remove(tellFunc);
      }
    }
    G_tellfile = output;
    G_tellfunc = prolog.G_user;
  }

  final void seen() {
     //Prolog.dump(G_seefile+"entering seen"+G_seeFiles);
    if (G_seefile != input) {
      Integer seeFunc = new Integer(G_seefunc);
      PrologReader file = (PrologReader)G_seeFiles.get(seeFunc);
       //Prolog.dump("seen="+file);
      if (file != null) {
        try {
          file.close();
          G_seeFiles.remove(seeFunc);
          //Prolog.dump("closed"+file);
        }
        catch (IOException e) {
          JavaIO.warnmes("error closing file:" + G_seefile);
        } // No action.

      }
      //else {
      //  JavaIO.errmes("seen/9 warning: file not open: "+G_seefunc);      
      //}
    }
    G_seefile = input;
    G_seefunc = prolog.G_user;
  }


  public void setInput(InputStream IS) {
    G_seefile = JavaIO.toReader(IS);
  }

  public void resetInput() {
    G_seefile = input;
  }

  public void setOutput(OutputStream OS) {
    G_tellfile = JavaIO.toWriter(OS);
  }


  public void resetOutput() {
    G_tellfile = output;
  }

  protected final void new_fluent() throws PrologException {

    xval = X(1);
    if (isINTEGER(xval)) {
      ires = OUTPUT_INT(xval);
    }
    else
      ires = 0;

    //prolog.dump("new_fluent created: "+ires);
    Object O = null;
    String fname = getAtomName(X(2));
    if (0 == ires) {
      O = JavaIO.toReader(fname);
    }
    else if (1 == ires) {
      O = JavaIO.toClauseReader(fname);
    }
    else if (2 == ires) {
      O = JavaIO.string2PrologReader(fname); //string stream
    }
    else if (3 == ires) {
      O = JavaIO.string2PrologClauseStringReader(fname); //string clause stream
    }
    else if (4 == ires) {
      O = Extender.toTokenReader(fname); //token stream
    }
    else if (5 == ires) {
      O = Extender.string2TokenReader(fname); //token stream
    }
    if (null == O)
      throw new ExistenceException("operation not implemented in: NEW_FLUENT_3");
    ires = addObject(O);
    xval = INPUT_INT(ires);
    OUT(xval);
  }

  void sread()  {
    String sclause = null;
    xval = X(3);
    if (isINTEGER(xval) && OUTPUT_INT(xval) == 1) {
      sclause = G_seefile.nextClauseString();
      int c = ' ';
      do { // consumes eol
        c = G_seefile.read();
      }
      while (Character.isWhitespace((char)c) && c != JavaIO.XNL);
    }
    else if (isSYMCONST(xval)) {
      sclause = getAtomName(xval);
    }
    else
      xval = 0;
    if (xval != 0) {
      xval = readTerm(
        sclause,
        prolog.G_true == X(1),
        prolog.G_true == X(2)
        );
    }
    if (xval == 0) {
      FAILURE();
    }
    else
      OUT(xval);
  }

  protected TermConverter newTermConverter(Prolog prolog) {
    return new TermReader(prolog, this);
  }

  public int readTerm(String s, boolean warnSingletons, boolean verbose) {
    return ((TermReader)termReader).parse(s, warnSingletons, verbose);
  }

  /*
  public Object stringToExternal(String s, boolean warnSingletons, boolean verbose) {
    //this.clear();
    try {
      int ref = readTerm(s, warnSingletons, verbose);
      Object O = toExternal(ref);
      if (null == O) return null;
      Fun TVs = (Fun)O;
      return TVs.args[0];
    }
    catch (Exception e) {
      return null;
    }
  }
  */

  private int canonicalTerm(int xref) throws TypeException,ResourceException {
    return prolog.atomTable.newFunctor(canonicalTermToString(xref), 0);
  }

  void swrite() throws PrologException {
    xval = canonicalTerm(X(1));
    OUT(xval);
  }

  protected void write_nl_io(int op) {
    switch (op) {
      /* no need to deref - termToString will do it
         note that regs[0] is not necessarily a VAR - it can be an int or const as well
      */
      case CWRITE_1: // $IO     
        print(termToString(regs[0]));
        instrPtr++;
        break;

      case NL_0: // $IO
        nl();
        instrPtr++;
        break;
      default: { }
    }
  }

  protected void seen_told_io(int op) {
    switch (op) {
      case SEEN_0: //$IO
        seen();
        instrPtr++;
        break;

      case TOLD_0: //$IO
        told();
        instrPtr++;
        break;

      default: { }
    }
  }

  protected void byte_io(int op) throws TypeException {
    switch (op) {
      case GET0_1: //$IO
        ires = G_seefile.read(); // $$ applet
        OUT(INPUT_INT(ires));
        break;

      case PUT0_1: // $$IO
        xval = X(1);
        if (!isINTEGER(xval)) {
          throw new TypeException("integer expected in put/1: " + dumpCell(xval));
          //warn_mes(xval + " integer expected in put/1");
          //FAILURE();
          //continue;
        }
        G_tellfile.write((char)OUTPUT_INT(xval));
        instrPtr++;
        break;
      default: { }
    }
  }

  protected void string_io(int op) throws PrologException {
    switch (op) {
      case SREAD0_4: //$IO
        sread();
        break;

      case SWRITE_2: //$IO
        swrite();
        break;
      default: { }
    }
  }

  protected void file_io(int op) throws PrologException {
    switch (op) {
      case SEEING_1: //$IO
        OUT(G_seefunc);
        break;

      case TELLING_1: //$IO
        OUT(G_tellfunc);
        break;

      case SEE_OR_FAIL_1: //$IO
        if (!see(X(1))) {
          FAILURE();
          break;
        }
        instrPtr++;
        break;

      case TELL_OR_FAIL_1: //$IO
        if (!tell(X(1))) {
          FAILURE();
          break;
        }
        instrPtr++;
        break;

      default: { }
    }
  }

  protected void shell() throws PrologException {
    shell_1(regs[1]);
    regs[1] = regs[2];
    instrPtr++;
  }

  private void shell_1(int hRef) throws PrologException {
    if (isNONVAR(hRef))
      xval = hRef;
    else
      deref(hRef); // gives xref, xval

    String buf = getAtomName(xval);
    JavaIO.println(JavaIO.runCommand(buf));
  }

  protected void refl_op(int op) {
    switch (op) {
      case NEW_JAVA_CLASS_2:
        xval = extender.new_java_class(X(1));
        if (xval == 0)
          FAILURE();
        else
          OUT(xval);
        break;

      case NEW_JAVA_OBJECT_3:
        xval = extender.new_java_object(X(1), X(2));
        if (xval == 0)
          FAILURE();
        else
          OUT(xval);
        break;

      case INVOKE_JAVA_METHOD_5:
        xval = extender.invoke_java_method(X(1), X(2), X(3), X(4));
        if (xval == 0)
          FAILURE();
        else
          OUT(xval);
        break;

      case DELETE_JAVA_CLASS_2:
        xval = extender.delete_java_class(X(1)) ?
          prolog.G_true
          : prolog.G_fail;
        OUT(xval);
        break;

      case DELETE_JAVA_OBJECT_2:
        xval = extender.delete_java_object(X(1)) ?
          prolog.G_true
          : prolog.G_fail;
        OUT(xval);
        break;

      case GET_JAVA_FIELD_HANDLE_3:
        xval = extender.get_java_field_handle(X(1), X(2));
        if (xval == 0)
          FAILURE();
        else
          OUT(xval);
        break;

      default: { }
    }
  }


  public final void write_instr(int target, int op, int reg, String fun, int arity) throws PrologException {
    CodeIO.write_instr(
    G_tellfile,
    op,
    reg,
    fun,
    arity
   );
  }

  protected boolean stop_impure(Object O) {
    if (O instanceof PrologReader) {
      ((PrologReader)O).stop(this);
      return true;
    }
    return false;
  }

  protected boolean impure_get(Object O) throws PrologException {
    if (O instanceof PrologReader) {
      PrologReader R = (PrologReader)O;
      //Prolog.dump("engine_get from Prolog reader:"+R+":"+R.getClass());
      xref = R.get(this);
      if (0 == xref) {
        R.stop(this);
        xval = prolog.atomTable.newFunctor("no", 0);
      }
      else {
        xval = prolog.atomTable.newFunctor("the", 1);
        xval = pushTerm(xval);
        pushTerm(xref);
      }
      return true;
    }
    else return false;
  }

  public String canonicalTermToString(int xref) {
    return safeTermToString(xref, true);
  }

  public String termToString(int xref) {
    return safeTermToString(xref, false);
  }

  private String safeTermToString(int xref, boolean canonical) {
    //sbuf.setLength(0);
    StringBuffer sbuf = new StringBuffer();
    try {
      genericTermToString(xref, sbuf, canonical);
    }
    catch (Exception e) {
      sbuf.setLength(40);
      String mes = "EXCEPTION: " + e + "=> " + sbuf + "...";
      JavaIO.errmes(mes, e);
      return "bad_term_" + xref;
    }
    return sbuf.toString();
  }

  /** prints a term given as a heap reference **/
  private void genericTermToString(int xref, StringBuffer sbuf,
                            boolean canonical) throws ResourceException {
    termToStringInternal(xref, sbuf, 0, canonical);
  }





  private int formatPrecision = 0;

  public void setFormatPrecision(int n) {
    formatPrecision = n;
  }

  private String double2string(double d) {
    return double2string(d, this.formatPrecision);
  }

  static public String double2string(double d, int prec) {
    if (prec > 0) {
      NumberFormat nf = NumberFormat.getInstance();
      nf.setMaximumFractionDigits(prec);
      return nf.format(d);
    }
    else
      return "" + d;
  }

  private void termToStringInternal(int xref, StringBuffer sbuf,
                                    int depth, boolean canonical) throws ResourceException {
    depth = checkDepth(depth);

    int xval;

    //Prolog.dump(">>xref:"+xref);
    if (isVAR(xref)) {
      deref(xref);
      xref = this.xref;
      xval = this.xval;
    }
    else
      xval = xref;

    //Prolog.dump("<<xref:"+xref);

    if (isVAR(xval)) {
      sbuf.append(termReader.getVarName(xval));
      return;
    }

    if (isINTEGER(xval)) {
      sbuf.append(OUTPUT_INT(xval));
      return;
    }

    if (isIDENTIFIER(xval)) {

      if (GETARITY(xval) == 0) {
        sbuf.append(termReader.toQuoted(xval));
        return;
      }
      /*
      if (xval == prolog.G_OBJECT) { // wrapped object  
       deref(xref+1);
       xref=this.xref;
       xval=this.xval;
       int handle=OUTPUT_INT(xval);
       Object O=prolog.atomTable.i2o(handle);
       String s;
       if(null!=O) s="'"+O.toString()+"'";
       else s="'"+getAtomName(prolog.G_OBJECT)+"'("+handle+")";
       sbuf.append(s);
       return;
      }
      */
      if (xval == prolog.G_FLOAT) { // FLOAT
        deref(xref + 1);
        int i1 = this.xval;
        deref(xref + 2);
        int i2 = this.xval;
        deref(xref + 3);
        int res = this.xval;
        double d = TermConverter.toDouble(i1, i2, res);
        if (formatPrecision > 0)
          sbuf.append(double2string(d));
        else
          sbuf.append(d);
        return;
      }

      /* do not do this - loses reversibility !!! //$STRING
     if (xval == prolog.G_STRING_CS2S) {
        deref(xref+1);
        sbuf.append("'TODO $STRING: termToStringInternal: paste list of chars into string'");   
        return;
     }
     */

      if ((!canonical) && (xval == prolog.G_VAR)) { // $VAR
        deref(xref + 1);
        int v = this.xval;
        if (isINTEGER(v)) {
          int i = OUTPUT_INT(v);
          sbuf.append("_" + i);
          return;
        }
      }

      if (xval == prolog.G_DOT) { // LIST
        sbuf.append("[");
        termToStringInternal(++xref, sbuf, depth, canonical);
        xref++;
        if (isVAR(xref)) {
          deref(xref);
          xref = this.xref;
          xval = this.xval;
        }
        else
          xval = xref;
        while (prolog.G_DOT == xval) {
          sbuf.append(",");
          termToStringInternal(++xref, sbuf, depth, canonical);
          xref++;
          if (isVAR(xref)) {
            deref(xref);
            xref = this.xref;
            xval = this.xval;
          }
          else
            xval = xref;
        }
        if (prolog.G_NIL != xval) {
          sbuf.append("|");
          termToStringInternal(xref, sbuf, depth, canonical); // should be xref !!!
        }
        sbuf.append("]");
        return;
      }

      // operators - for now all xfy and static

      String fun = getAtomName(xval);

      if (!canonical) {
        String info = (String)TermReader.opTable.get(fun);

        if (GETARITY(xval) == 2 && null != info) {
          boolean needsPars = info.equals("atom_op");
          if (needsPars) sbuf.append("(");
          opToString(getRef(xref + 1), xval, false, sbuf, depth, canonical);
          //opToString(xref+1,xval,false,sbuf);
          sbuf.append(" " + fun + " ");
          opToString(getRef(xref + 2), xval, true, sbuf, depth, canonical);
          //opToString(xref+2,xval,true,sbuf);
          if (needsPars) sbuf.append(")");
          return;
        }
      }
      // general functor.
      sbuf.append(termReader.toQuoted(fun));
      sbuf.append("(");
      int arity = GETARITY(xval);
      for (int i = 1; i <= arity; i++) {
        termToStringInternal(getRef(xref + i), sbuf, depth, canonical);
        if (i < arity) sbuf.append(",");
      }
      sbuf.append(")");
    }
    else {
      sbuf.append("bad_data(" + xval + ")");
    }
  }


  private void opToString(int xref, int parent, boolean right, StringBuffer sbuf,
                  int depth, boolean canonical) throws ResourceException {
    depth = checkDepth(depth);

    int xval;
    if (isVAR(xref)) {
      deref(xref);
      xref = this.xref;
      xval = this.xval;
    }
    else
      xval = xref;

    if (!isIDENTIFIER(xval) || 2 != GETARITY(xval)) {
      termToStringInternal(xref, sbuf, depth, canonical);
      return;
    }

    String fun = getAtomName(xval);
    String info = (String)TermReader.opTable.get(fun);

    if (null == info) {
      termToStringInternal(xref, sbuf, depth, canonical);
      return;
    }

    boolean needsPars = !right || (xval != parent);
    if (needsPars) sbuf.append("(");
    opToString(getRef(xref + 1), xval, false, sbuf, depth, canonical);
    sbuf.append(" " + fun + " ");
    opToString(getRef(xref + 2), xval, true, sbuf, depth, canonical);
    if (needsPars) sbuf.append(")");
  }

  public void serialize(int hRef) throws PrologException {
    if (isNONVAR(hRef))
      xval = hRef;
    else
      deref(hRef); // gives xref, xval

    String fname = getAtomName(xval) + ".jc";

    Prolog.dump("serializing to:" + fname);
    //Extender.toFile(fname,this);
    Extender.toFile(fname, prolog);
  }

  final public static double divide(int a, int b) {
    if(0==b) {Interact.errmes("tryng to divide "+a+" by zero"); return 0.0d;}
    return ((double)a)/((double)b);
  }

  public static void sleep(long s) {
    sleep_ms(1000L*s);
  }
  
  public static void sleep_ms(long ms) {
    try {
      Thread.sleep(ms);   
    }
    catch(InterruptedException e) {
    }
  }
  
} // end class Machine

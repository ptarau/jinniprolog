package prolog.kernel;
import prolog.logic.*;

/**
 * Prolog DCG-style parser and tokenizer in Java
 */
public class TermReader extends TermConverter {
  public TermReader(Prolog prolog, HeapStack heap) {
     super(prolog,heap);
  }

  private String charIn;
  private int cursorMax;
  private int cursor;
  private char curChar;

  final void checkDepth() throws ResourceException {
    //Prolog.dump("depth="+this.depth);
    this.depth = HeapStack.checkDepth(this.depth);
  }

  int parse(String s, boolean warnSingletons, boolean verbose) {
    try {
      return parse1(s, warnSingletons && Top.get_verbosity() >= 2);
    }
    catch (PrologException e) {
      if (verbose && Top.get_verbosity() >= 1) {
        int l = s.length();
        int a = this.cursor - 20; a = Math.min(a, l); a = Math.max(0, a);
        int z = this.cursor + 20; z = Math.min(z, l);
        int i = Math.min(a, 40); i = Math.min(i, a);
        String si = "*** " + s.substring(0, i) + " ... " + JavaIO.NL;
        String sf = ">>>" + JavaIO.NL + s.substring(a, z) + JavaIO.NL + "<<<";
        JavaIO.warnmes(JavaIO.NL + si + sf + JavaIO.NL + e.toString() + JavaIO.NL);
      }
      return 0;
    }
  }

  private final void initParser(String charIn) {
    this.charIn = charIn;
    this.cursorMax = charIn.length() - 1;
    this.cursor = -1;
    advChar();
  }

  private final String fixInput(String s) {
    String eof = "end_of_file";
    if (null == s) s = eof;
    else if (s.equals("user")) {
      //s=JavaIO.readln();
      s = ((Machine)this.heap).readln();
      if (null == s) {
        s = eof;
      }
    }

    s = s.trim(); int l = s.length() - 1;
    if (l >= 0 && s.charAt(l) == '.') s = s.substring(0, l);
    if (s.length() == 0) return null;
    return s + " ";
  }

  private final int parse1(String s, boolean warnSingletons) throws PrologException {
    s = fixInput(s);
    if (null == s) return 0;
    int clause = 0;
    clear();
    initParser(s);

    //skipWhitespace();  String s=this.charIn;
    try {
      clause = match_clause();
      skipWhitespace();
    }
    catch (StringIndexOutOfBoundsException ex) {
      readError("string index error:" + ex);
    }
    if (cursor < cursorMax) clause = 0;
    if (0 == clause) //Prolog.dump("error parsing at:"+curChar+":"+cursor);
      readError("parser left unconsumed input");

    ObjectIterator e = varTable.getKeys();

    IntStack varStack = new IntStack();
    while (e.hasNext()) {
      String key = (String)e.next();
      int name = putConst(key);
      int var = ((Integer)varTable.get(key)).intValue();
      int[] args ={ name, var };
      varStack.push(putFun("=", args));
    }
    int vars = varStack.toList(heap);
    int[] args ={ clause, vars };
    int result = putFun("-", args);

    e = occTable.getKeys();
    if (warnSingletons) {
      StringBuffer b = new StringBuffer();
      int ctr = 0;
      while (e.hasNext()) {
        Object O = e.next();
        if (O instanceof String) {
          String key = (String)O;
          if ('_' == key.charAt(0)) continue;
          b.append("<" + key + "> ");
          ctr++;
        }
      }
      if (ctr > 0)
        Prolog.dump(b + " *** warning: singleton variables in: "
                         + heap.termToString(clause) + JavaIO.NL);
    }
    clear();
    return result;
  }

  /**
   * advaces on character
   */
  private final char advChar() {
    //Prolog.dump(">"+curChar);
    curChar = charIn.charAt(++cursor);
    return curChar;
  }

  private final boolean eof() {
    return cursor >= cursorMax;
  }

  private char nextChar() {
    return charIn.charAt(cursor + 1);
  }

  /*
  private final boolean eoln() {
    //char next; 
    return eof() || JavaIO.XNL == nextChar();
  }
  */
  
  private final void backtrack(int where) {
    cursor = where; // backtrack
    curChar = charIn.charAt(cursor);
  }

  /*********************** LEXICAL ANALYSER *********************/
  private final void skipWhitespace() throws SyntaxException {
    for (; ; ) {
      if (eof()) break;
      else if (Character.isWhitespace(curChar)) {
        while (Character.isWhitespace(curChar) && !eof()) advChar();
      }
      /*
      else if(curChar=='%') {
        while(!eoln() && !eof()) advChar();
        if(eoln()) advChar();
      }
      else if(curChar=='/' && !eof() && nextChar()=='*') {
        // Prolog.dump("begin comment!"+curChar);
        advChar();
        while(!eof() && !(curChar=='*' && nextChar()=='/') && !(curChar=='/' && nextChar()=='*'))
          advChar();
        boolean ok=!eof() && curChar=='*'&& '/'==nextChar();
        if(ok) advChar(); 
        if(!eof()) advChar();
        //Prolog.dump("end comment!"+curChar+nextChar()+":"+ok);
        if(eof() || !ok) readError("EOF or embedded comment in / * .. * / comment");
      }
      */
      else
        break;
    }
  }
  private final boolean ALNUM() {
    boolean b = (Character.isLetterOrDigit(curChar) || curChar == '_') && !eof();
    if (b) advChar();
    return b;
  }

  private final boolean LOWER() {
    boolean b = Character.isLowerCase(curChar) && !eof();
    if (b) advChar();
    return b;
  }
  private final boolean UPPER() {
    boolean b = (Character.isUpperCase(curChar) || curChar == '_') && !eof();
    if (b) advChar();
    return b;
  }

  private final boolean DIGIT() {
    boolean b = Character.isDigit(curChar) && !eof();
    if (b) advChar();
    return b;
  }

  // static recognizer

  private static final String punctuation = "!?@#$&:></~`*-+=\\|^$";
  private static final String specials = ".-=><";

  private final boolean PUNCT() {
    boolean b = isPUNCT(curChar) && !eof();
    if (b) advChar();
    return b;
  }

  public static final boolean isALNUM(char c) {
    return (Character.isLetterOrDigit(c) || c == '_');
  }

  public static final boolean isPUNCT(char c) {
    return punctuation.indexOf(c) != -1;
  }

  public static final boolean isSPECIAL(char c) {
    return specials.indexOf(c) != -1;
  }

 

  /****************************** PARSER ****************************/
  /* (actually a way to translate DCGs to C, for lazy programmers)  */

  /* backtrackable operations: build no terms */

  private boolean match_char(char c) throws SyntaxException {
    skipWhitespace();
    return match_char0(c);
  }

  private boolean match_char0(char c) throws SyntaxException {
    boolean b = (c == curChar) && !eof();
    if (b) advChar();
    return b;
  }
  private boolean match_string(String s) throws SyntaxException {
    skipWhitespace();
    int from = cursor;
    int l = s.length();
    //if(cursor+l>=charIn.length() || isPUNCT(charIn.charAt(cursor+l))) return false;
    if (cursor + l >= charIn.length()) return false;
    //Prolog.dump("first after STRING "+s+"!"+charIn.charAt(cursor+l));
    String match = charIn.substring(cursor, cursor + l);
    boolean ok = match.equals(s);
    //Prolog.dump("<"+s+">"+ok+"=<"+match+">");
    if (!ok) return false;
    cursor += l;
    curChar = charIn.charAt(cursor);
    //Prolog.dump("first after STRING "+s+"#"+curChar);
    if (isSPECIAL(curChar)) {
      backtrack(from);
      return false;
    }
    return true;
  }

  static final private char q = '\'';

  private String match_sym() throws SyntaxException {
    int from, to;
    skipWhitespace();
    //System.err.println("match_sym!:" + (int)curChar+":");    	

    from = cursor;
    if (LOWER()) {
      while (ALNUM())
        ;
      to = cursor;
      return charIn.substring(from, to);
    }
    else if (PUNCT()) {
      while (PUNCT()) ;
      to = cursor;
      return charIn.substring(from, to);
    }
    else if (match_char(q)) {
      //Prolog.dump("first after QUOTE:"+curChar);
      StringBuffer s = new StringBuffer();
      for (; ; ) {
        int pos = charIn.indexOf(q, cursor);
        //Prolog.dump(from+"POS OF NEXT QUOTE"+pos+":"+charIn.charAt(pos));
        if (-1 == pos) {
          curChar = charIn.charAt(cursor);
          Prolog.dump("closing quote not found stopping at" + charIn.charAt(pos));
          return null;
        }
        s.append(charIn.substring(cursor, pos));


        cursor = pos + 1;
        curChar = charIn.charAt(cursor);
        //Prolog.dump("mach_sym_quote_double_quote:"+curChar);
        if (curChar == q) {
          s.append(q);
          curChar = charIn.charAt(++cursor);
          continue;
        }
        break;
      }
      //Prolog.dump("end_mach_sym_quote:"+s);
      return s.toString();
    }
    else {
      backtrack(from);
      return null;
    }
  }

  private String match_var() throws SyntaxException {
    int from, to;
    skipWhitespace();
    //System.err.println("match_var:" + curChar);
    from = cursor;
    if (UPPER()) {
      while (ALNUM())
        ;
      to = cursor;
      return charIn.substring(from, to);
    }
    else {
      backtrack(from);
      return null;
    }
  }

  private String match_int() throws SyntaxException {
    skipWhitespace();
    return match_int0();
  }

  private String match_int0() throws SyntaxException {
    int from, to;
    //System.err.println("match_int:" + curChar);
    from = cursor;
    match_char('-');
    if (DIGIT()) {
      while (DIGIT())
        ;
      to = cursor;

      String s = charIn.substring(from, to);
      return s;
    }
    else {
      backtrack(from);
      return null;
    }
  }

  private String match_float() throws SyntaxException {
    int from, to;
    //System.err.println("match_int:" + curChar);
    from = cursor;
    match_char('-');
    if (DIGIT()) {
      while (DIGIT())
        ;
      if (match_char0('.') && DIGIT()) {
        while (DIGIT())
          ;
      }
      else {
        backtrack(from);
        return null;
      }
      to = cursor;

      String s = charIn.substring(from, to);
      return s;
    }
    else {
      backtrack(from);
      return null;
    }
  }

  private int match_chars() throws SyntaxException {
    //skipWhitespace(); // do not skip them - we are in "..."
    IntStack buf = new IntStack();
    //StringBuffer s=new StringBuffer("<");
    while (!eof()) {
      if (curChar == '\"') {
        if (nextChar() == '\"') advChar();
        else break;
      }
      //s.append((char)curChar);
      buf.push(putInt(curChar));
      advChar();
    }
    return buf.toList(heap);
  }

  /** 
   * compile time clause level operators - all xfy at this point
  */
  public static String[] atom_ops ={
                     "-->",":-",
                     ";",
                     "->",
                     ","
};

  /**
   * compile time term level operators - all xfy at this point
   */
  public static String[] term_ops ={
                     ":","@","#","=>","<=", // higher - for handling OO extensions
                     "#=>","<=#",
                     "==>","<==",
                     ">","<","=",
                     "=<",">=","==","\\==","=\\=","=:=",
                     "=..",
                     "@<","@>","@=<","@>=",
                     "is",
                     "is_bigint",
                     "is_bigdec",
                     "-",
                     "+",
                     "/","*","mod","//",
                     ">>","<<",
                     "^"
};

  static public ObjectDict opTable = new ObjectDict();

  static {
    for (int i = 0; i < atom_ops.length; i++) {
      opTable.put(atom_ops[i], "atom_op");
    }
    for (int i = 0; i < term_ops.length; i++) {
      opTable.put(term_ops[i], "term_op");
    }
  }


  private int match_clause() throws PrologException {
    int c = 0;
    if (match_string(":-")) {
      //if(match_string("multifile")) {
      //return 0;
      //}
      //if(match_string("dynamic")) {
      //return 0;
      //}
      int body = match_with_atom_ops(atom_ops, 0);
      int[] args ={ body };
      c = putFun(":-", args);
    }
    else
      c = match_with_atom_ops(atom_ops, 0);
    //Prolog.dump("result:"+c+heap.termToString(c));
    return c;
  }

  private int match_with_atom_ops(String[] ops, int i) throws PrologException {
    //Prolog.dump(ops[i]+":"+i+":"+curChar);
    if (i >= ops.length) return match_top_term();
    int h = match_with_atom_ops(ops, i + 1);
    if (match_string(ops[i])) {
      int b = match_with_atom_ops(ops, i);
      return make_fun2(ops[i], h, b);
    }
    else
      return h;
  }

  // $$ possible bug
  public static String[] prefixops ={ "\\+" };

  private int match_top_term() throws PrologException {

    for (int k = 0; k < prefixops.length; k++) {
      String prefixop = prefixops[k];
      if (match_string(prefixop)) {
        checkDepth();
        int body = match_top_term();
        depth--;
        int[] args ={ body };
        return putFun(prefixop, args);
      }
    }

    return match_term();
  }

  private int match_with_term_ops(String[] ops, int i) throws PrologException {
    //Prolog.dump(ops[i]+":"+i+":"+curChar);
    checkDepth();
    if (i >= ops.length) return match_simple_term();
    int h = match_with_term_ops(ops, i + 1);
    if (match_string(ops[i])) {
      int b = match_with_term_ops(ops, i);
      return make_fun2(ops[i], h, b);
    }
    else
      return h;
  }

  public final int make_fun2(String f, int t1, int t2) throws PrologException {
    int[] args ={ t1, t2 };
    return putFun(f, args);
  }

  private final int cons(int hd, int tl) throws PrologException {
    return make_fun2(".", hd, tl);
  }

  private int match_term() throws PrologException {
    int d = this.depth;
    checkDepth();
    int r = match_with_term_ops(term_ops, 0);
    this.depth = d;
    return r;
  }

  private int match_simple_term() throws PrologException {
    int t;
    //System.err.println("match_term:" + curChar);
    if (match_char('[')) {
      //skipWhitespace();
      if (match_char(']')) {
        return putConst("[]");
      }
      else {
        int hd = match_term();
        int tl = match_list_elements();
        if (hd != 0 && tl != 0) {
          return cons(hd, tl);
        }
        else
          readError("error in match_term");
      }
    }

    if (match_char('(')) {
      //skipWhitespace();
      if (match_char(';') && match_char(')')) return putConst(";");
      if (match_char(',') && match_char(')')) return putConst(",");
      int c = match_clause();
      if (match_char(')')) return c;
      readError("error in embedded term");
    }

    if (match_char('{')) {
      //skipWhitespace();
      int c = match_clause();
      if (match_char('}')) {
        int args[] ={ c };
        return putFun("{}", args);
      }
      readError("error in {..} term");
    }

    if (match_char('"')) {
      //skipWhitespace();
      int s = match_chars(); // !! should be more general
      if (match_char('"')) return s;
      readError("error in double quted term");
    }

    String var = match_var();
    if (var != null) {
      return putVar(var);
    }

    String fval = match_float();
    if (fval != null) {
      try {
        return putFloat(fval);
      }
      catch (NumberFormatException e) {
        JavaIO.warnmes("unparsable matched DECIMAL: <" + fval + ">");
        fval = null;
      }
    }

    String val = match_int();
    int ival;
    if (val != null) {
      try {
        ival=Integer.parseInt(val);
        if((ival>=Defs.MAXINT) || (ival< -Defs.MAXINT)) return putConst(val);
      }
      catch (NumberFormatException e) {
        JavaIO.warnmes("unconvertible matched INT left as string: <'" + val + "'>");
        return putConst(val);
      }
      return putInt(ival);
    }

    char term_prefix_op = curChar;

    // leads to bug when something '?'(A) is used as (A) is not recognized
    if (match_char0('~') || match_char0('$') || match_char0('#') || match_char0('@') || match_char0('`') || match_char0('?')) { // || match_char0('+') || match_char0('-') || match_char0('*')) { - leads to bug
      // if(','==nextChar()) return putConst(""+term_prefix_op);
      int body = match_simple_term();
      int[] args ={ body };
      return putFun("" + term_prefix_op, args);
    }

    String sym = match_sym();
    if (sym != null) {
      //System.err.println("\tmatched SYM:" + sym);		
      if (match_char('(')) {
        int first = match_term();
        if (first != 0) {
          IntStack stack = new IntStack(4);
          stack.push(first);
          int arg = 1;
          arg = match_args(arg, stack);
          t = putFun(sym, arg);
          if (arg != 0) {
            //int[] argvect=stack.toArray();      
            //for (int i = 1; i <= arg; i++) {
            //	heap.pushTerm(argvect[i - 1]);
            //}	

            putArgs(stack.toArray());
            return t;
          }
          else readError("error in compound term");
        }
        return first;
      }
      else {
        return putConst(sym);
      }
    }
    else {
      readError("error in match_sym");
      return 0; //never happens
    }
  }

  private int match_args(int arg, IntStack stack) throws PrologException {
    //System.err.println("match_args:" + curChar);
    if (match_char(')')) { }
    else
      if (match_char(',')) {
        int t = match_term();
        //argvect[arg] = t;
        stack.push(t);
        if (t != 0)
          //arg = match_args(++arg, argvect);
          arg = match_args(++arg, stack);
      }
      else
        arg = 0; // unexpected
    return arg;
  }

  private int match_list_elements() throws PrologException {
    checkDepth();
    //System.err.println("match_list:" + curChar);
    int t = 0;
    int hd;//, tl;
    if (match_char(']')) {
      t = prolog.G_NIL;
    }
    else if (match_char('|')) {
      hd = match_term();
      if (match_char(']')) {
        t = hd;
      }
      else
        readError("<,> expected in match_list_elements");
    }
    else if (match_char(',')) {
      hd = match_term();
      //tl = match_list_elements(); t = cons(hd, tl);
      t = match_more_list_elements(hd);
    }
    else {
      readError("<,|> expected in match_list_elements");
    }
    depth--;
    return t;
  }

  private int match_more_list_elements(int hd) throws PrologException {
    IntStack ls = new IntStack();
    ls.push(hd);
    while (match_char(',')) {
      hd = match_term();
      ls.push(hd);
    }
    int tl = match_list_elements();
    return ls.toList(heap, tl);
  }

  /**
  * error handler: tries to return text right after culprit
  */
  private void readError(String message) throws SyntaxException {
    int to = Math.min(cursor + 40, charIn.length() - 1);
    throw new SyntaxException(message + "=>" + JavaIO.NL + "... " + charIn.substring(cursor, to) + " ...");
  }
}

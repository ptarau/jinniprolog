package prolog.logic;

/**
 * Prolog DCG-style parser and tokenizer in Java
 */
public class TermConverter implements ITerm, Stateful {

  protected HeapStack heap;

  protected Prolog prolog;
  AtomTable atomTable;
  protected ObjectDict varTable, occTable;
  
  protected int depth;

  public TermConverter(Prolog prolog, HeapStack heap) {
    this.prolog = prolog;
    this.atomTable = prolog.atomTable;
    this.heap = heap;
    varTable = new ObjectDict();
    occTable = new ObjectDict();
    depth = 0;
  }

  public void clear() {
    varTable.clear();
    occTable.clear();
    depth = 0;
  }

  public void destroy() {
    varTable = null;
    occTable = null;
    atomTable = null;
    prolog = null;
  }

  public String getVarName(int xval) {
    String s = "_" + xval;
    if (null == occTable) return s;
    Object O = occTable.get(new Integer(xval));
    if (null == O || !(O instanceof String)) return s;
    String name = (String)O;
    //if(name.length()>0 && name.charAt(0)=='_') return name;
    return name;
  }
  // ITerm interface implementation:

  /**
   * part of ITerm interface: creates a Var
   */
  public int putVar(Object name) {
    Object VX = varTable.get(name);
    Integer V;
    if (null != VX) {
      V = (Integer)VX;
      occTable.remove(name);
      return V.intValue();
    }
    int v = heap.newVar();
    V = new Integer(v);
    if (hasName(name)) {
      varTable.put(name, V);
      occTable.put(name, name);
      occTable.put(V, name);
    }
    return v;
  }

  boolean hasName(Object O) {
    //return (O instanceof String) &&
    //       ((String)O).charAt(0)!='_';
    return !O.equals("_");
  }

  /**
   * Part of ITerm interface: creates a constant. Internalizes the string,
   * assuming a different Prolog as a client. A slightly faster
   * put_fun(String name, int arity) + put_args(int[] args) could
   * optimize the case when symbols are shared.
   */
  public int putConst(String s)  throws PrologException {
    //return heap.pushTerm(atomTable.inputTerm(s,0));
    return putFun(s, 0);
  }

  /**
   * part of ITerm interface: creates an integer
   */
  public int putInt(int i) {
    return heap.pushTerm(Defs.INPUT_INT(i));
  }

  // putFloat operations

  final public int putFloat(String s) throws TypeException {
    double d = parseDouble(s);
    return putFloat(d);
  }

  private static final int chop = 3; //??Defs.getTagBits()+1;
  private static long CHOP_MASK = (1L << (chop << 1)) - 1;

  static final long doubleToLongBits(double d) {
    return Double.doubleToLongBits(d);
  }

  static final double longBitsToDouble(long bits64) {
    return Double.longBitsToDouble(bits64);
  }
  
  final public int putFloat(double d) {
    long bits64 = doubleToLongBits(d);
    long lres = bits64 & CHOP_MASK;
    bits64 = (bits64 >>> (chop << 1)) << chop;
    long left = bits64 >>> 32;
    long right = ((bits64 << 32) >>> 32);
    right = right >>> chop;

    int i = (int)left;
    int dec = (int)right;
    int res = (int)lres;

    //double d1=toDouble(i,dec,res);
    //Prolog.dump("putFloat!!!:"+d+"="+d1);
    //Prolog.dump("bits64  !!!:"+Double.doubleToLongBits(d)+"="+Double.doubleToLongBits(d1));

    return putFloatParts(
      Defs.INPUT_INT(i),
      Defs.INPUT_INT(dec),
      Defs.INPUT_INT(res)
    );
  }

  public static final double toDouble(int i1, int i2, int i3) {
    long left = Defs.OUTPUT_INT(i1);
    long right = Defs.OUTPUT_INT(i2);
    long res = Defs.OUTPUT_INT(i3);
    left = left << 32;
    right = right << chop;
    long bits64 = left | right;
    bits64 = bits64 << chop;
    bits64 = bits64 | res;
    double d = longBitsToDouble(bits64);
    return d;
  }

  private final int putFloatParts(int i, int dec, int res) {
    int xref = heap.pushTerm(prolog.G_FLOAT);
    putInt(i);
    putInt(dec);
    putInt(res);
    return xref;
  }



  static final double parseDouble(String s) throws TypeException {
    try {
      return Double.valueOf(s).doubleValue();
    }
    catch (NumberFormatException e) {
      throw new TypeException("number expected in parseDouble, found: " + s);
    }
  }

  /**
   * Builds a list of chars representation of a string.
   * Main purpose: avoid internalizing "transient" constants.
   */
  public final int putString(String s) throws PrologException {
    return heap.string2list(s);
  }

  /**
   * Part of ITerm interface: creates a compound term
   * It assumes args are already built and collected in the args array
   * in this implementation we push refs to them after building the functor.
   * As a result, this interface accomodates the usual bottom-up term building of the WAM
   * and can be used to attach other Prologs with different term representaion
   * as well as to devices like term readers.
   */
  public int putFun(String f, int[] args) throws PrologException {
    int fun = putFun(f, args.length);
    putArgs(args);
    return fun;
  }

  // the next 2 operations are not part of the interface - still using them makes
  // code more uniform and portable

  final public int putFun(String f, int arity) throws PrologException {
    int fun = atomTable.newFunctor(f, arity);
    return heap.pushTerm(fun);
  }

  final public void putArgs(int[] args) {
    // can be made faster with arrayCopy if needed - to be provided as an operation by heap
    for (int i = 0; i < args.length; i++) {
      heap.pushTerm(args[i]);
    }
  }

  final private int putOneObject(Object o) throws SyntaxException {
    int arg = Defs.INPUT_INT(atomTable.addObject(o));
    //Prolog.dump("putObject="+o+"=>"+args[0]);
    //return putFun("$object",args);
    int xref = heap.pushTerm(heap.prolog.G_OBJECT);
    heap.pushTerm(arg);
    return xref;
  }

  public int putObject(Object O) throws PrologException {
    clear();
    return putObjectInternal(O);
  }

  final private int putObjectInternal(Object O) throws PrologException {
    //Interact.println("result="+O);
    int res;
    if (null == O) res = heap.prolog.G_null;
    else if (O instanceof Integer)
      res = putInt(((Integer)O).intValue());
    else if (O instanceof Double)
      res = putFloat(((Double)O).doubleValue());
    else if (O instanceof String)
      res = putConst((String)O);
    else if (O instanceof Var) {
      res = putVar(O);
    }
    else if (O instanceof HeapRef) { // created with G_ENCODED
      res = ((HeapRef)O).getRef();
    }
    else if (O instanceof EncodedTerm) { // created with G_ENCODED
      res = heap.decodedCopy((EncodedTerm)O);
    }
    else if (O instanceof PortableTerm) { // created with G_PORTABLE
      EncodedTerm T = ((PortableTerm)O).outputTo(prolog);
      res = heap.decodedCopy(T);
    }
    else if (O instanceof Fun) {
      Fun F = (Fun)O;
      int l = F.args.length;
      int[] iargs = new int[l];
      if (1 == l && prolog.G_STRING_S2CS.equals(F.name)) {
        res = putString((String)F.args[0]);
      }
      else {
        for (int i = 0; i < F.args.length; i++) {
          iargs[i] = putObjectInternal(F.args[i]);
        }
        res = putFun(F.name, iargs);
      }
    }
    else
      res = putOneObject(O);
    return res;
  }

  public static Fun object_to_codes(Object O) {
    return new Fun(Prolog.G_STRING_S2CS, O.toString());
  }

  /*
  public static Fun object_to_codes(int radix, java.math.BigInteger O) {
    return new Fun(Prolog.G_STRING_S2CS, O.toString(radix));
  }
  */

  public static Fun object_to_codes(int len, int radix, java.math.BigInteger O) {
    String S=O.toString(radix);
    int pad=Math.abs(len)-S.length();
    if (0==len||0==pad) {} // do nothing
    else if(pad<0) {S="?"+S+"?";} // show error
    else if (len>0) {
      StringBuffer b=new StringBuffer(S);
      for (int i=0;i<pad;i++) {
        b.append('0');
      }
      S=b.toString();
    }
    else if (len<0) {
      StringBuffer b=new StringBuffer();
      for (int i=0;i<pad;i++) {
        b.append('0');
      }
      b.append(S);
      S=b.toString();
    }
    return new Fun(Prolog.G_STRING_S2CS, S);
  }

  /**
  * Part of the ITerm interface: digests an external Object and returns an int handle to it
  */

  /**
   *part of ITerm interface:  builds external terms through the OTerms interface
   */

  public Object getTerm(int xref) throws PrologException {
    return getTerm(xref, heap);
  }

  public Object getTerm(int xref, OTerm O) throws PrologException {
    //clear(); //???
    return getTermInternal(xref, O);
  }

  private Object getTermInternal(int xref, OTerm O) throws PrologException {
    if (0 == xref) convertError("putTerm: variable cannot be 0");
    int xval;
    if (Defs.isVAR(xref)) {
      heap.deref(xref); // assumes this is a VAR
      xref = heap.xref;
      xval = heap.xval;
    }
    else
      xval = xref;
    Object R;
    if (Defs.isVAR(xval))
      R = O.putVar(xval);
    else if (Defs.isINTEGER(xval))
      R = O.putInt(Defs.OUTPUT_INT(xval));
    else if (Defs.isIDENTIFIER(xval)) {
      String f = heap.getAtomName(xval);
      int arity = Defs.GETARITY(xval);
      //Prolog.dump("xref:"+xref+"->xval:"+xval+":"+f+"/"+arity);
      if (0 == arity) R = O.putConst(f);
      else if (1 == arity && xval == prolog.G_OBJECT) {
        int arg = xref + 1;
        heap.deref(arg);
        if (Defs.isINTEGER(heap.xval)) {
          int handle = Defs.OUTPUT_INT(heap.xval);
          R = atomTable.i2o(handle);
        }
        else
          R = null;

        //Prolog.dump("f="+f+" handle="+handle+" object="+R);
        if (null == R) convertError("bad object handle in getTermInternal(): " + heap.xval);
      }
      else if (1 == arity && xval == prolog.G_ENCODED) {
        int arg = xref + 1;
        heap.deref(arg);
        R = heap.encodedCopy(heap.xref);
      }
      else if (1 == arity && xval == prolog.G_PORTABLE) {
        int arg = xref + 1;
        heap.deref(arg);
        EncodedTerm T = heap.encodedCopy(heap.xref);
        R = new PortableTerm(T, prolog);
      }
      else if (1 == arity && xval == prolog.G_STRING_CS2S) { // $STRING_CS2S $STRING
        int arg = xref + 1;
        heap.deref(arg);
        String S = heap.list2buf(heap.xref, heap.xval);
        //R=O.putConst("TODO $STRING: getTermInternal: convert list of chars to string");
        R = O.putConst(S);
      }
      else if (3 == arity && xval == prolog.G_FLOAT) {
        int arg = xref + 1;
        heap.deref(arg);
        int i = heap.xval;
        arg = xref + 2;
        heap.deref(arg);
        int dec = heap.xval;
        arg = xref + 3;
        heap.deref(arg);
        int res = heap.xval;
        R = O.putFloat(toDouble(i, dec, res));
      }
      else {
        Object[] args = new Object[arity];
        for (int i = 0; i < arity; i++) {
          args[i] = getTerm(xref + 1 + i, O);
        }
        R = O.putFun(f, args);
      }
    }
    else {
      R = null;
      convertError("bad data in putTerm:" + xref + "->" + xval);
    }
    return R;
  }

  private void convertError(String mes) {
    Interact.errmes(mes);
  }

  /*
  int hash2alist(ObjectDict htable, String cons) throws PrologException {
    clear();
    ObjectIterator e = htable.getKeys();
    IntStack eStack = new IntStack();
    while (e.hasNext()) {
      Object key = e.next();
      int name = putObjectInternal(key);
      int val = putObjectInternal(htable.get(key));
      int[] args ={ name, val };
      eStack.push(putFun(cons, args));
    }
    return eStack.toList(heap);
  }
  */

  public String toQuoted(int xval) {
    String s = atomTable.getAtomName(xval);
    if (null == s) return null;
    return toQuoted(s);
  }

  public static String toQuoted(String s) {
    char quote = '\'';

    // empty string needs quotes!
    if (s.length() == 0) return quote + s + quote;

    char a = s.charAt(0);

    // check if already quoted
    if (a == quote) {
      if (1 == s.length()) s = quote + s + s + quote; // ' ==> ''''
      return s;
    }
    boolean needsq = false;
    if ((Character.isUpperCase(a) ||
        Character.isDigit(a) ||
        '_' == a
       )
      )
      needsq = true;
    else if (s.equals("!")) { /* without quotes */ }
    else
      for (int i = 0; i < s.length(); i++) {
        char c = s.charAt(i);
        //if(!isPUNCT(c) && c!=quote) continue;
        //if (isALNUM(c)) continue; // || Character.isWhitespace(c)) 
        // same as ^^  
        if (Character.isLetterOrDigit(c) || c == '_') continue;
        needsq = true;
        break;
      }
    if (needsq) {
      StringBuffer sb = new StringBuffer();
      for (int i = 0; i < s.length(); i++) {
        char c = s.charAt(i);
        if (quote == c) {
          sb.append(quote); //double it
        }
        sb.append(c);
      }
      s = "\'" + sb + "\'";
    }
    return s;
  }
}

package prolog.logic;

/** The Prolog Heap.
*<p><b>Term Representation</b>
*<p>Terms are represented as single integers in the following format.
*<p>
* <table border=1>
* <tr><td>integer<td>INTTAG<td colspan=2>value
* <tr><td colspan=4>&nbsp
* <tr><td>atom<td>FUNTAG<td>symbol number<td>0
* <tr><td colspan=4>&nbsp
* <tr><td>functor<td>FUNTAG<td>symbol number<td>arity
* <tr><td colspan=4>&nbsp
* <tr><td>variable<td>VARTAG<td colspan=2>heap index
* </table>
*<br>Note that VARTAG is zero so that VAR terms are just heap indexes.
*<p>
*Design note: This represents a reversal in the order of the sub-fields from the C version.
* For efficient operation in Java, the VAR type must represent a simple heap index. Having the
* TAG bits at the high order end of the term achieves this. This will unfortunately lead to a
* slight performance hit on RISC machines.
*/
public class HeapStack extends Defs implements OTerm {
  /** Access to the current Prolog Shared data. */
  public Prolog prolog;

  public TermConverter termReader;

  /** The heap storage. */
  protected int[] cells;
  ///** The maximum size of the heap. */
  //private int heapMax;

  /** The first used element in the heap.
  * heapBase should be set to zero for compatibility with the C version of this program.
  * Otherwise instruction traces will not correspond.
  * Normally BASE should be set to 1 so that element zero is never used.
  * This means that it should be possible to detect zero stores.
  */
  /** The current top of the heap. Unlike the C version, this represents the last used element */
  private int heapTop;
  private int heapBase;

  /** ref/val pair used for dereferencing terms. */
  protected int xref;
  protected int xval;

  //private HRef hRef = new HRef();

  public static final int MINSIZE = 1 << 16; // no shrinking to smaller size

  /**
  * Create a new heap.
  * @param prolog The Prolog instance with which this heap is associated.
  * @param atomMax The required maximum size of the heap.
  */


  HeapStack(Prolog prolog, int heapsize) {
    this.prolog = prolog;
    heapBase = 0; // Element 0 is never used (except in error), as ++ is performed first in push
    heapTop = heapBase;
    cells = new int[heapsize];
    termReader = newTermConverter(prolog);
  }

  protected TermConverter newTermConverter(Prolog prolog) {
    return new TermConverter(prolog, this);
  }

  public static long mmTIME = 0;
  private long oneMMtime = 0;

  final public void startMMtimer() {
    oneMMtime = System.currentTimeMillis();
  }

  final public void endMMtimer() {
    oneMMtime = System.currentTimeMillis() - oneMMtime;
    mmTIME += oneMMtime;
  }

  /**
   * dynamic array operation: heap expander
   */
  protected void expand() {
    startMMtimer();
    //Prolog.dump("heap expanding: "+heapTop);
    int l = cells.length;
    int[] newstack = new int[l<<1];
    if (PrologGC.trace >= 2) Prolog.dump("heap shrinking: " + (l << 1));
    System.arraycopy(cells, 0, newstack, 0, l);
    cells = newstack;
    endMMtimer();
  }

  /**
   * dynamic array operation: heap reducer
   */
  protected void shrink() {
    int l = cells.length;
    if (l <= MINSIZE || heapTop << 2 >= l) return;
    l = 1 + (heapTop << 1); //this still means shrink to 1/2 of the heap or less
    if (heapTop < MINSIZE) l = MINSIZE;
    if (PrologGC.trace >= 2) Prolog.dump("heap shrinking: " + l);
    int[] newstack = new int[l];
    System.arraycopy(cells, 0, newstack, 0, heapTop + 1);
    cells = newstack;
  }

  /**
  * Push a term value onto the heap. First it increments and 
  * then it assigns - this means that heapTop points to the last assigned cell
  * and that the next available cell will be at heapTop+1.
  * @param term The term to be added.
  * @return The heap index of the added term.
  */
  public final int pushTerm(int t) {

    // if(heapTop+1>=cells.length) expand(); cells[++heapTop]=t;

    ++heapTop;
    try {
      cells[heapTop] = t;
    }
    catch (ArrayIndexOutOfBoundsException ignore) {
      expand();
      cells[heapTop] = t;
    }

    return heapTop;
  }

  final int newVar() {
    return pushTerm(heapTop + 1);
  }

  /** Set the heap top the indicated value.
  * This should only be called from ChoicePointStack during a backtrack operation.
  */
  final void setHeapTop(int newTop) { heapTop = newTop; }

  /** Return the current top of the heap.*/
  final int getHeapTop() { return heapTop; }

  /** Utility function to push a []/0 term. */
  final void pushNil() { pushTerm(prolog.G_NIL); }

  /** Utility function to push a ./0 term and a specified term.
  * @param term The term to be added.
  */
  final void pushList(int term) {
    pushTerm(heapTop + 2); // prevents bug in unify and maybe elsewhere $$ ??
    pushTerm(prolog.G_DOT);
    pushTerm(term);
  }

  /**
   * Pushes subarray of cells to heap
   */
  final void pushCells(int source[], int from, int arity) {
    if (arity + heapTop >= cells.length - 1) expand();
    System.arraycopy(source, from, cells, heapTop + 1, arity);
    heapTop += arity;
  }

  /** Set a heap element to the given term.
  * @param hRef The heap index to be set.
  * @param term The term to replace the heap element at <code>hRef</code> heap index.
  */
  final void setRef(int hRef, int term) {
    cells[hRef] = term;
  }

  /** Return the term at a given heap index.
  * @param hRef The heap index.
  * @return The indicated heap term.
  */
  public final int getRef(int hRef) { return cells[hRef]; }


  /** Completely clear the heap.*/
  public void clear() {
    heapTop = heapBase; //this points to invalid data: a var can never be 0;
  }

  public void destroy() {
    clear();
    cells = null;
    termReader.destroy();
  }

  /** Return the count of entries.*/
  public final int getUsed() { return heapTop + 1; }


  /** Return available heap entries.*/
  public final int getFree() { return cells.length - heapTop - 1; }

  /*
  final void fullDeref() { // similar to FDEREF - but more precise - would trigger error if derefed compound f/n
    if(isATOMIC(xref)) xval=xref;
    else deref();
    // xval contains either a constant or an integer or a functor or an unbound var
    // xref is assumed to stay VAR if it pointed to a compound
  }
  */

  final void fullDeref() { FDEREF(); }

  final void FDEREF() {
    if (isNONVAR(xref))
      xval = xref;
    else {
      deref();
    }
  }

  final void deref() {
    // ASSERT isVAR(xref)
    while (isVAR(xval = cells[xref]) && xref != xval) { // variable and not unbound.	
      xref = xval;
    }
    // ASSERT isVAR(xref) and xval contains either a non-var or an unbound var
    // in any case, cell[xref] contains the same thing as xval
  }

  public final void deref(int xref) {
    this.xref = xref;
    this.xval = xref;
    // ASSERT isVAR(xref)
    deref();
  }

  int copyTermFrom(HeapStack M, int t) {
    EncodedTerm ET = M.encodedCopy(t);
    t = decodedCopy(ET);
    ET.destroy();
    return t;
  }

  public EncodedTerm encodedCopy(int t) {
    int h = heapTop;
    //Prolog.dump(heapTop+":orig to encode:"+termToString(t));
    int ct = copyTerm(t);
    //Prolog.dump(heapTop+":copy to encode:"+termToString(ct));
    EncodedTerm T;
    if (isATOMIC(ct))
      T = new EncodedTerm(ct);
    else
      T = new EncodedTerm(cells, ct, heapTop + 1);
    //Prolog.dump("encodedTerm:"+T+"size:"+T.size()+"="+(heapTop-h));

    heapTop = h;
    return T;
  }

  int decodedCopy(EncodedTerm T) {
    //if(T.size()==2) return T.getRef(1);
    while (getFree() <= T.size()) expand();
    //expand
    int ct = heapTop + 1;
    heapTop = T.decodeTerm(ct, cells);
    return ct;
  }

  private ObjectDict varTable;

  final int copyTerm(int t) {
    if (getFree() <= getUsed()) expand();
    int currentTop = heapTop;
    varTable = new ObjectDict();

    int ct = t;

    try {
      ct = xcp(t);
    }
    catch (ResourceException e) {
      heapTop = currentTop;
      Interact.warnmes("copy_term warning: recursion overflow, returned: " + prolog.S_null);
      ct = prolog.G_null;
    }
    varTable = null;

    return ct;
  }

  private final int xcp(int xref) throws ResourceException {
    IntStack copyStack = new IntStack();

    int handle = newVar();

    copyStack.push(xref);    // term to be copied
    copyStack.push(handle); // where to put the copy

    while (!copyStack.isEmpty()) {

      int ct = copyStack.pop(); // where to put the copy
      xref = copyStack.pop();   // term to be copied

      // dereference
      int xval;

      if (isVAR(xref)) {
        deref(xref);
        xref = this.xref;
        xval = this.xval;
      }
      else
        xval = xref;

      // handle simple terms

      int cval = copySimple(xval);

      if (cval != 0) {
        setRef(ct, cval);
      }
      else {
        // handle compound terms
        int arity = GETARITY(xval);
        int h = pushTerm(xval);
        setRef(ct, h);
        /*
         for (int i=1; i<arity+1; i++) {
           h=newVar();
           copyStack.push(xref+i); // term to be copied
           copyStack.push(h);  // where to put the copy
         }
        */
        for (int i = arity; i > 0; i--) {
          //newVar(); // just because it possibly expands heap
          copyStack.push(xref + i); // term to be copied
          copyStack.push(h + i);  // where to put the copy
        }
        heapTop += arity;
        if (heapTop + 1 >= cells.length) expand();
      }
    }
    return handle;
  }

  final int copySimple(int xval) {

    // handle variables
    if (isVAR(xval)) {
      Integer Key = new Integer(xval);
      Object XVal = varTable.get(Key);
      Integer Val;
      if (null == XVal) {
        int newvar = newVar();
        Val = new Integer(newvar);
        varTable.put(Key, Val);
      }
      else Val = (Integer)XVal;
      return Val.intValue();
    }

    //handle integers
    if (isINTEGER(xval)) return xval;

    // handle atoms
    int arity = GETARITY(xval);
    if (arity == 0) return xval;

    return 0;
  }


  final int termHash(int t) {

    int h = -1;

    try {
      h = xh(t);
    }
    catch (ResourceException e) {
      Interact.warnmes("term_hash warning: resource overflow on very large term");
    }

    return Defs.INPUT_INT(h);
  }

  private final int xh(int xref) throws ResourceException {
    IntStack copyStack = new IntStack();
    copyStack.push(xref);    // term to be hashed
    int hcode = 0;

    while (!copyStack.isEmpty()) {

      xref = copyStack.pop();

      // dereference
      int xval;

      if (isVAR(xref)) {
        deref(xref);
        xref = this.xref;
        xval = this.xval;
      }
      else
        xval = xref;

      // Prolog.dump("hash_term: "+dumpCell(xval));

      if (Defs.isVAR(xval)) {
        return -1;
      }

      int k = 0;

      if (Defs.isINTEGER(xval)) {
        k = Defs.OUTPUT_INT(xval);
      }
      else if (Defs.isIDENTIFIER(xval)) {
        int arity = GETARITY(xval);
        String s = getAtomName(xval);
        k = AtomTable.string_hash(s, arity);
        for (int i = arity; i > 0; i--) {
          copyStack.push(xref + i); // term to be hashed
        }
      }
      else {
        Interact.warnmes("hash_term BAD TAG: " + dumpCell(xval));
        return -2;
      }

      k = Math.abs(k);
      k = k & ((1 << 29) - 1);

      hcode = (hcode << 4) + k;

    }

    hcode = hcode & ((1 << 29) - 1);
    hcode = Math.abs(hcode);
    return hcode;
  }

  public static final int checkDepth(int depth) throws ResourceException {
    if (depth > Interact.RECURSION_DEPTH) throw
         new ResourceException("recursion depth limit reached in I/O opertion, max=" + Interact.RECURSION_DEPTH);
    else
      return depth + 1;
  }

  final String list2buf(int l, int vl) {
    // get heap address of start of list.
    StringBuffer sbuf = new StringBuffer();
    while (vl == prolog.G_DOT) {
      deref(++l);
      int carVal = xval;
      if (isINTEGER(carVal))
        sbuf = sbuf.append((char)OUTPUT_INT(carVal));
      else {
        return null;
      }
      deref(++l);
      l = xref;
      vl = xval;
    }
    if (vl != prolog.G_NIL) {
      return null;
    }
    return sbuf.toString();
  }

  final int string2list(String sbuf) {
    int r = getHeapTop() + 1;
    for (int i = 0; i < sbuf.length(); i++) {
      pushList(INPUT_INT(sbuf.charAt(i)));
    }
    pushNil();
    return r;
  }

  public final String getAtomName(int getAtomName) {
    return prolog.atomTable.getAtomName(getAtomName);
  }

  /**
 * returns the string image of a cell
 */
  private String dumpCell0(int x) {
    String sbuf;
    if (Defs.isIDENTIFIER(x))
      sbuf = termReader.toQuoted(x) + "/" + GETARITY(x);
    else
      sbuf = Defs.showCell(x);
    return sbuf;
  }

  public String dumpCell(int x) {
    try {
      return dumpCell0(x);
    }
    catch(Exception e) {
      return "bad_cell#" + x;
    }
  }
  
  /**
   * returns a human readable representation of a heap cell
   * at a given index as a String
   */
  String cellToString(int i) {
    String sbuf;
    try {
      int x = cells[i];
      sbuf = dumpCell(x);
    }
    catch (Exception e) {
      sbuf = "bad_cell_address#" + i;
    }
    return sbuf;
  }

  /*  simple representation of a reference as String 
   *  overridden in Machine with a recursive printing of a term
   */
  public String termToString(int xref) {
    return cellToString(xref);
  }

  public void dump() {
    int limit = 20;
    dump((heapTop > limit) ? (heapTop - limit) : 0, heapTop);
  }

  public void dump(int from, int to) {
    if (Prolog.DEBUG) {
      Prolog.dump("HEAP used:" + heapTop + " max:" + cells.length);
      for (int i = from; i < to && i <= heapTop; i++) {
        if (cells[i] != 0) {
          Prolog.dump("[" + i + "]=" + cellToString(i));
        }
      }
      Prolog.dump("END HEAP.");
    }
  }

  // ITerm operations: for portable prolog to Prolog translation

  public Object toExternal(int ref) {
    try {
      return termReader.getTerm(ref, this);
    }
    catch (PrologException e) {
      return null;
    }
  }

  public Object putVar(int i) throws PrologException {
    return new Var(i);
  }
  public Object putConst(String c) {
    return c;
  }

  public Object putInt(int i) {
    return new Integer(i);
  }

  public Object putFun(String f, Object[] args) {
    return new Fun(f, args);
  }

  public Object putFloat(double d) {
    return new Double(d);
  }

  public int getTerm(Object t, ITerm I) throws PrologException {
    throw new SystemException("unexpected/unimplemented getTerm in HeapStack");
  }
} // End class HeapStack


package prolog.logic;
import java.util.Random;

/**
 *  Implements a Prolog code space associated to byte code file loaded in
 *  a CodeStore and an AtomTable - returning unique integers for various Java
 *  Objects - in particular for Java Strings naming functors and constants
 */
public final class Prolog implements Stateful 
{
  private int instance_id;
  
  private void clear_instance_id() {
    instance_id=0;
  }
  
  /*
    also in Machine - therefore ambiguous - if called 
    in a Machine sets the machine's id to this
    can be called on classes or engines
  */
  public int new_instance_id() {
    return ++instance_id;
  }
 
  public String get_class_name() {
    return className;
  }
 
  private static Prolog defaultProlog;

  private static Prolog origProlog;
  
  private static final Prolog cloneProlog(Prolog prolog,String className) {
    Prolog newprolog;
      try {
        newprolog=prolog.cloneFor(className);
      }
      catch(PrologException e) {
        Interact.errmes("error in cloning prolog",e);
        newprolog=prolog; //// attempting next best
      }
    return newprolog;
  }
  
  /**
   * cloned by Prolog classes
   * synchronized to allow multiple parallel new(..) operations for
   * Prolog objects and agents
   */
  synchronized public static Prolog cloneOrigProlog(String className) {
    if(null==origProlog) {
       origProlog=cloneProlog(getDefaultProlog(),"origProlog");
    }
    return cloneProlog(origProlog,className);
  }
  
  public static final Prolog getDefaultProlog()  {
    if(null==defaultProlog) {
       defaultProlog=getProlog(null);
    }

    /*
    System.err.println(
        "defaultProlog: code=>"+defaultProlog.codeStore.getUsed() +
        ",dict size=>"+defaultProlog.dict.getUsed());
    
    System.err.println(
        "origProlog: code=>"+origProlog.codeStore.getUsed() +
        ",dict size=>"+origProlog.dict.getUsed());
    */
    
    return defaultProlog;
  }

  public static final void setDefaultProlog(Prolog prolog)  {
    defaultProlog=prolog;
  }
  

  /**
    de facto external constructor - constructs and returns Prolog class
    as a result of loading a bytecode file returns null if fails
  */
  private static final Prolog getProlog(String fname)  {
    try {
      return new Prolog(fname);
    }
    catch(PrologException e) {
      Interact.errmes("failing to construct Prolog instance from bytecode file: "+fname,e);
      return null;
    }
  }
   
  // Class variables - COMPILE TIME data
  
	static final boolean DEBUG = false;
	static final boolean TRACE = DEBUG;
	static boolean TRACE_ON = false;
	static int instrCount=0;
	static int TRACE_START = 0;
	static int TRACE_STOP = 0;
  
	// timeStamp values.
	public static final byte LOADTIME = 0;
	public static final byte RUNTIME = 1;
	public static final byte BBOARDTIME = 2;
	
  public final static Integer[] stamps={new Integer(LOADTIME),new Integer(RUNTIME),new Integer(BBOARDTIME)};
  
  public Integer getMark() {
    if(timeStamp<stamps.length) return stamps[timeStamp];
    return new Integer(timeStamp);
  }
  
	public byte timeStamp = LOADTIME;
  
  private static int codeMax=1<<15,MAXDICT=1,MAXATOM=1;
  
  private static ObjectDict classTable=new ObjectDict();
  
  synchronized public static final void addPrologClass(String name,Prolog prolog) {
    if(null!=classTable.get(name)) 
      Interact.warnmes("Prolog class already exists: "+name);
    else 
      classTable.put(name,prolog); 
  }
  
  synchronized public static final Prolog getPrologClass(String name) {
    return (Prolog)classTable.get(name); 
  }
  
  synchronized public static final void removePrologClass(String name) {
    Prolog prolog=getPrologClass(name);
    if(null!=prolog) {
      classTable.remove(name); //should go first
      prolog.stop();
    }
  }
  
  /*
    fixes possible initialization problems after serialization
  */
  public void refresh() {
    //codeStore.initInstructionLengths();
  }
  
  private void initDataAreas() throws SystemException {
    //this(null);
	 try {
    
		atomTable = new AtomTable(this, MAXATOM); // should be first
    
    init_constants(); // should be after atomTable as it uses it 
    
    dict = new Dict(this,MAXDICT); // should be after initConstants as it uses them
    
    codeStore = new CodeStore(this, codeMax); // uses atomTable, constants and dict
    
    random= new Random();
    
    atomTable.addObject(this);
    
    propDict=new ObjectDict();

    clear_instance_id();
	 }
	 catch (Exception e) {
	   Interact.errmes("Fatal system error in initDataAreas",e);
     throw new SystemException("Prolog Initalization Error");
	 } 
  }

  /* props API  - these props survive compile operations that reset Dict data */

  public void setProp(Object K1,Object K2,Object V) {
    if(null==K1 || null==K2 || null==V) Interact.warnmes(
      "null in Prolog.setProp("+K1+","+K2+","+V+")");
    Term K=new Term(new Fun("x",K1,K2));
    propDict.put(K,V);
  }

  public Object getProp(Object K1,Object K2) {
    if(null==K1 || null==K2) Interact.warnmes(
      "null in Prolog.getProp("+K1+","+K2+")");
    Term K=new Term(new Fun("x",K1,K2));
    return propDict.get(K);
  }

  public void clearProps() {
    propDict=new ObjectDict();
  }

  public ObjectIterator propIterator() {
    return propDict.getKeys();
  }

  private void initFromFile(String wamFile) throws PrologException {
    initDataAreas(); // should be first, it creates atomTable, constants, dicts, codeStore
    //Prolog.dump("init from:"+wamFile);
    load(wamFile);    
    className="prolog";
    //atomTable.addObject(this); // done in this()
    addPrologClass(className,this);
  }
   
  
  /**
   * Cereates runtime system and loads byte code file given as argument.
   */
  private Prolog(String wamFile) throws PrologException {
    if(null==wamFile) wamFile=Interact.PROLOG_BYTECODE_FILE;
    initFromFile(wamFile);
  }
   
  /**
   * Creates runtime system and loads default byte code file.
   *
  *
  public Prolog() throws PrologException {
    this(null);
  }
  */
  
  synchronized Prolog cloneFor(String className) throws PrologException {
    // problem if classname has already .pl - from find_file ?
    
    Prolog source=getPrologClass(className);
    if(null!=source) return source; // already loaded and cloned ###
    
    source=this;
    //Prolog.dump("not found in classTable: "+ className);
                         
    Prolog other=null;
    try {
      other=(Prolog)source.clone();
      //other.atomTable is shared because all Java Objects are there
      other.dict = source.dict.cloneWith(other);
      other.codeStore = source.codeStore.cloneWith(other);
    }
    catch(CloneNotSupportedException e) {
      Interact.errmes("error cloning: "+this,e);
      //never happens - they all do support it...
    }
    if(null==other) 
      throw new SystemException("failing to create instance of: "+className);
    
    if(!className.equals("prolog")) {    
      atomTable.addObject(other); // fixes bug - otherwise Machine cannot use reflection
      other.className=className;
      addPrologClass(className,other);
      other.clear_instance_id();
    }
    return other;
  }
 
  /** 
   *  Instance variables - RUNTIME data
   */
  
  String className;
  
  Dict dict;
	public AtomTable atomTable;
  CodeStore codeStore;
  ObjectDict propDict;

	int rtime;
	long startTime;
  private Random random;
 
  public int G_true;
  public int G_fail;
  public static String S_null="$null";
  public int G_null; /* representation for Java's null */
  
  public int G_empty;
  public int G_ref;
 
  public int G_predmark;
  public int G_addrmark; 
  public int G_undefined;
  public int G_metacall;
	public int G_prolog_main;
	
  public int G_NIL;
	public int G_DOT;
	public int G_DIF;
  public int G_FLOAT;
  public int G_STRING_CS2S;
  public static String G_STRING_S2CS="$STRING_S2CS";
  public int G_OBJECT;
  public int G_ENCODED;
  public int G_PORTABLE;
  public int G_VAR;
  public int G_user;
  public int[] compare_vals;
   
  public void init_constants() throws PrologException {
    G_true = atomTable.newFunctor("true", 0); // should be the first
    G_fail = atomTable.newFunctor("fail", 0);
    G_null = atomTable.newFunctor(S_null, 0);
    G_empty = atomTable.newFunctor("$empty", 0);
    G_ref = atomTable.newFunctor("$ref", 0);
	
		G_prolog_main = atomTable.newFunctor("run", 3);
		G_predmark = atomTable.newFunctor("predmark", 0);
    G_addrmark = atomTable.newFunctor("addrmark", 0);
    G_undefined = atomTable.newFunctor("$undefined", 3);
    G_metacall = atomTable.newFunctor("metacall", 2);
    
		G_NIL = atomTable.newFunctor("[]", 0);
		G_DOT = atomTable.newFunctor(".", 2);
		G_DIF = atomTable.newFunctor("-", 2);
    // special functors - to force various external encodings
		G_FLOAT = atomTable.newFunctor("$float", 3);
    G_STRING_CS2S = atomTable.newFunctor("$STRING_CS2S", 1);
    
    G_OBJECT = atomTable.newFunctor("$object", 1);
    G_ENCODED = atomTable.newFunctor("$encoded", 1);
    G_PORTABLE = atomTable.newFunctor("$portable", 1);
    G_VAR = atomTable.newFunctor("$VAR", 1);
    G_user = atomTable.newFunctor("user", 0);
    
    compare_vals=new int[3];
  
    compare_vals[0] = atomTable.newFunctor("<", 0);
		compare_vals[1] = atomTable.newFunctor("=", 0);
		compare_vals[2] = atomTable.newFunctor(">", 0);	 
  }
  
  public int getRandom() {
    return Math.abs(random.nextInt()>>2);
  }
  
  public void setRandom(int seed) {
    random=Tools.newRandom(seed);
  }
  
  public final boolean load(String fName) throws LoadException {
    boolean ok=false;
    try {
      ok=codeStore.load(fName);
    }
    catch(Exception e) {
      throw new LoadException("Error in loading: "+fName+"=>"+e);
    }
    timeStamp=RUNTIME;
  	startTime = System.currentTimeMillis();
  	rtime=0;
    return ok;
  }

  public final void stop() {
    atomTable.removeObject(this);
    //Prolog other=getPrologClass(className);
    //if(other==this) 
    removePrologClass(className); //to be replaced with ref counting ???
    className=null;
    dict=null;
	  atomTable=null;
    codeStore=null;
    compare_vals=null;
  }

  // needs more work
  public final void advance_code_top() {
    codeStore.setTopBack();
  }
  
  public final void rollback() throws PrologException {
    // not needed as commit is not implemented
    codeStore.resetTopBak(); // as it was at LOADTIME - for sure
    codeStore.rollback();
    dict.rollback(LOADTIME); 
  }
  
  public final void hard_rollback() throws PrologException {
    rollback();
    atomTable.rollback(LOADTIME);
  }

  public static void dump(Object s) {
    Interact.println("!!!"+s);
  }
  
  public String handle2string(int i) {
    Object o=atomTable.i2o(i);
    String s;
    if(null==o) s=S_null;
    
    if(o.equals(this)) s=className+"@"+hashCode();
    else s=o.toString();
    
    return "'"+s+"'";
    
  }
  
  public String toString() {
    //return className+"@"+hashCode();
    return "'"+this.atomTable.getAtomName(this.G_OBJECT)+"'("+
      atomTable.o2i(this)+")";
  }
} 

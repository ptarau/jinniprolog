package prolog.logic;

/**
* Implements a value lookup hash-table. The value is keyed by
* two independent key values. The current implementation uses
* linear probing.
*/
final class Dict implements Stateful {

/** Access to the current prolog.kernel prolog. */
private Prolog prolog;

/** The Dictionary storage. */
private DictEntry[] entries;

/** The size of the dictionary. */
private int dictMax; // power of 2
private int dictLow;
private int dictHigh;
/** The number of entries currently in the dictionary. */
private int dictUsed;


/**
* Create a new dictionary.
* @param prolog The Prolog instance with which this dictionary is associated.
* @param dictMax The required size of the dictionary.
*/
public Dict(Prolog prolog, int dictMax) {
	//ASSERT: val is not 0, hput avoids duplications 
	this.prolog = prolog;
	this.dictMax = dictMax;
	entries = new DictEntry[dictMax];
  updateUsage();
}

synchronized Dict cloneWith(Prolog prolog) throws CloneNotSupportedException {
  Dict other=(Dict)this.clone();
  other.prolog=this.prolog;
 
  other.entries=(DictEntry[])entries.clone();
  
  //for(int i=0;i<entries.length;i++) {
  //  DictEntry e=other.entries[i];
  //  if(null!=e) other.entries[i]=e.toClone();
  //}
 
  // we do this because otherwise RUNTIME entries would
  // conflict - this.entries
  // -- no need for this if all entries are cloned
  
  //Prolog.dump("entries:@>>"+other.entries.hashCode());
  
  return other;
}


/**
 * Updates load info.
 */

private final void updateUsage() {
  dictLow=dictMax>>3; // 1/8 full
  dictHigh=dictMax>>1; // 1/2 full
  dictUsed=0;
}

/**
 * Dynamically adjusts the hashtable when more than 0.75% full 
 */

private final void rehash() throws PrologException {
  if(//dictUsed<(1<<13) || dictLow<(1<<12) &&
    (dictUsed>=dictLow && dictUsed<dictHigh)) 
  return;
  rehash0();
}

private final void rehash0() throws PrologException {
  //System.err.println("ENTERING rehash, used:"+dictUsed+
  //        " max:"+dictMax+" low:"+dictLow+" high: "+dictHigh);
  int oldMax=dictMax;
  if(dictUsed<dictLow) dictMax=dictMax>>1; // shrink to half if 1/8 used; => 25% full
  else dictMax=dictMax<<1; // double if 1/2 used => 25% full
  //int prevDictUsed=dictUsed;
  updateUsage();
  DictEntry[] oldDict=entries;
  entries=new DictEntry[dictMax];
 
  for(int level=0; level<=prolog.BBOARDTIME; level++) {
    //System.err.println("!!!REHASH: dictUsed:"+dictUsed+"!="+oldUsed+" level:!!!"+level);
    //int oldUsed=dictUsed;
    for(int j=0;j<oldMax;j++) {
      DictEntry entry=oldDict[j];
      if(null==entry || 
         entry.val==prolog.G_empty || 
         entry.timeStamp!=level ||
         (entry.val==prolog.G_ref && ((RefDictEntry)entry)==null)
         ) {
             //if(entry!=null) System.err.println("DROPPING:"+entry.val);
             continue;
           }
      int i=hget_internal(entries,dictUsed,entry.pred,entry.fun);
      // assertion: each entry should hash exactly once
      if(null!=entries[i]) throw new SystemException("error in rehash on:"+entry);
      entries[i]=entry;
      //if(entry.timeStamp>0 && entry.val!=prolog.G_ref) 
      //   System.err.println("adding entry: "+j+" "+entry);
      dictUsed++;
    }
    /* // BUG !!! 
    if(dictUsed==oldUsed && dictUsed>0) {
      //System.err.println("AFTER LEVEL:"+level+"dictUsed"+oldUsed+"==>"+dictUsed+
        " max:"+dictMax+" low:"+dictLow+" high: "+dictHigh);
      break; //BUG !!! - wrong to break - next level can still have dynamic database
    }
    */
  }
  //System.err.println("END rehash, used:"+prevDictUsed+"==>"+dictUsed+" max:"+dictMax+" low:"+dictLow+" high: "+dictHigh);
}

/**
* Compute hash value for specified keys.
* @param dictMax The size of this dictionary.
* @param key1 An integer id. representing a term or value.
* @param key2 An integer id. representing a term or value.
* @returns The integer hash code.
* <p>Implementation Note:
* <br>This is implemented as a static function to aid inlining by the compiler.
*/
//private final static int hashKeys(int dictMax, int key1, int key2) {
//	return Math.abs((key1 + key2) & (dictMax-1));
//}

private final static int hashKeys(int dictMax, int key1, int key2) {
  //return Math.abs(((key1<<2)^(key1>>11) + (key2<<5)^(key2>>17)) & (dictMax-1));
  return Math.abs(( (key1<<8)+ ((key2<<5)^key2) ) & (dictMax-1));
}

final boolean isFree(DictEntry entry) throws TypeException {
  if(null==entry) return true;
  if(entry.timeStamp<prolog.timeStamp) throw new TypeException("invalid update attempt to: "+entry);
  return entry.val==prolog.G_empty;
}

final void assertUpdatable(DictEntry entry) throws TypeException {
  if(null==entry||entry.val==prolog.G_empty||entry.timeStamp<prolog.timeStamp) 
    throw new TypeException("invalid update attempt to: "+entry);
}

/**
* Create a new dictionary entry. Entries are tagged with the current
* prolog epoch (time stamp).
* If an entry already exists then its value is <EM>not</EM> updated.
* @param pred A predicate id.
* @param func A function id.
* @param func The value id. to associate with the preceeding keys.
* @returns <code>true</code> if this is a new definition.
* @throws ResourceException if the dictionary is full.
*/
public boolean hdef(int pred, int fun, int val) throws PrologException {
  rehash();
	int i = hget_internal(entries, dictUsed, pred, fun);
	DictEntry entry = entries[i];
  if(isFree(entry)) {
    	entries[i]= new DictEntry(pred,fun,val,prolog); 
   	  if(null==entry) dictUsed++;
		  return true;
	}
	//Do not update val here! Use hSet if hDef succeeds,
	return false;
}


/**
* Sets the value of an exisiting dictionary entry.
* @param pred A predicate id.
* @param func A function id.
* @param func The new value id.
*/
public final void hset(int pred, int fun, int val) throws PrologException {
	int i = hget_internal(entries, dictUsed, pred, fun);
	DictEntry entry = entries[i];
	assertUpdatable(entry);
	entry.xsetVal(val);
}

public final void hremove(int pred, int fun) throws PrologException {
  hset(pred,fun,prolog.G_empty);
}

public final int getpred(int pred) throws PrologException {
   return hget(prolog.G_predmark,pred);
}

public final int addr2fun(int addr)  throws PrologException {
   return hget(prolog.G_addrmark,addr);
}

public final boolean setpred(int pred,int addr) throws PrologException {
   return hdef(prolog.G_predmark,pred,addr) && 
          hdef(prolog.G_addrmark,addr,pred);
}


/**
* Retrieves the value of an exisiting dictionary entry.
* @param pred A predicate id.
* @param func A function id.
* @returns The required value.
* @throws ExistenceException if an existing definition is not found.
*/
public final int hget(int pred, int fun) throws PrologException {
	int i = hget_internal(entries, dictUsed, pred, fun);
	DictEntry dictEntry = entries[i];
  if (dictEntry == null) // return 0;
    return prolog.G_empty;
	return dictEntry.val;
}

/**
* For internal use only.
* Retrieves a DictEntry or null.
* @param entries The dictionary.
* @param dictUsed Count of entries currently in the dictionary.
* @param pred A predicate id.
* @param func A function id.
* @returns <code>true</code> if this is a new definition.
* <p>Implementation Note:
* <br>This is implemented as a static function to aid inlining by the compiler.
*/
private static final int hget_internal(DictEntry[] entries, int dictUsed, int pred, int fun) throws ResourceException {
	int dictMax = entries.length;
	int i = hashKeys(dictMax, pred, fun);
  DictEntry dictEntry = entries[i];
	if (dictEntry != null && dictEntry.isFound(pred, fun))
		return i;
   	if (dictEntry == null) return i;
    int last = (i + dictUsed+1) % dictMax;
    while (true) {
      i = (i + 1) % dictMax;
      if (i == last) {
        throw new ResourceException("Dictionary is full (used="
           +dictUsed+", i:"+i+", last:"+last+", max:"+dictMax+")");
      }
      dictEntry = entries[i];
    	if (dictEntry == null || dictEntry.isFound(pred, fun)) 
        return i; // i is the index where this dictEntry _is_ or _should be_
    }
}

//

/**
* Create a new dictionary entry. Entries are tagged with the current
* prolog epoch (time stamp).
* If an entry already exists then its value is <EM>not</EM> updated.
* @param pred A predicate id.
* @param func A function id.
* @param func The value id. to associate with the preceeding keys.
* @returns <code>true</code> if this is a new definition.
* @throws ExistenceException if the dictionary is full.
*/
public final boolean hdef_ref(int pred, int fun, Object oref) throws PrologException {
  rehash();
	int i = hget_internal(entries, dictUsed, pred, fun);
  DictEntry entry = entries[i];
  if(isFree(entry)) {
    entries[i] = new RefDictEntry(pred,fun,oref,prolog);
		if(null==entry) dictUsed++;
		return true;
	}
	//Do not update val here! Use hSet if hDef succeeds,
	return false;
}


/**
* Sets the value of an exisiting dictionary entry.
* @param pred A predicate id.
* @param func A function id.
* @param func The new value id.
* @throws ExistenceException if an existing definition is not found.
*/
public final void hset_ref(int pred, int fun, Object oref) throws PrologException {
	int i = hget_internal(entries, dictUsed, pred, fun);
	DictEntry entry = entries[i];
	assertUpdatable(entry);
  if(entry.val==prolog.G_ref) {
    entry.xsetRef(oref);
  }
  else {
    entries[i]=new RefDictEntry(entry,oref);
  }
}

/**
* Retrieves the value of an exisiting dictionary entry.
* @param pred A predicate id.
* @param func A function id.
* @returns The required value.
* @throws ExistenceException if an existing definition is not found.
*/
public final Object hget_ref(int pred, int fun) throws ResourceException {
	int i = hget_internal(entries, dictUsed, pred, fun);
  return ref_at(i); 
} 

private final Object ref_at(int i) {
	RefDictEntry dictEntry = (RefDictEntry)entries[i];
  if (dictEntry == null)
		return null;
	return dictEntry.oref;
}


/**
* Removes entries from the dictionary which were created with a later epoch
* (time stamp) that the one specified.
* @param timeStamp Specified epoch.
*/
public void rollback(byte timeStamp) throws PrologException {
    for (int i = 0; i < dictMax; i++) {
      DictEntry dictEntry = entries[i];
		  if (dictEntry != null && dictEntry.timeStamp > timeStamp) {
		    dictUsed--;
        //System.err.println("wiped out:"+entries[i]);
			  entries[i] = null;
		  }
	  }	
    rehash();
}

/** Get dictionary entry count.*/
public final int getUsed() { return dictUsed; }

/** Get dictionary size.*/
public final int getMax() { return dictMax; }

/** Get dictionary available entries.*/
public final int getFree() { return dictMax-dictUsed; }


/*
* Dump specified range of dictionary entries.
*/
public void dump(CodeStore codeStore, int from, int to )  {
  	Prolog.dump("DICT used:" + dictUsed + " max:" + dictMax);
	for(int i=from; i<to && i<dictMax; i++) {
	    DictEntry dictEntry = entries[i];
	    if (dictEntry != null && dictEntry.timeStamp>0) {
	    	Prolog.dump("[" + i + "] "+dictEntry
        );    	
	    }
      else to++;
	}
  Prolog.dump("END DICT: $empty="+prolog.G_empty+",$oref="+prolog.G_ref+"  dictUsed:"+dictUsed);
}

public void dump() {
  dump(prolog.codeStore,0,100);
}

// builtins
  
  final boolean do_hdef(int k1,int k2,int val,HeapStack heap)  throws PrologException {
    prolog.timeStamp=prolog.BBOARDTIME;
    boolean ok;
    if(Defs.isATOMIC(val)) 
      ok= hdef(k1,k2,val);
    else 
      ok=hdef_ref(k1,k2,heap.encodedCopy(val));
    prolog.timeStamp=prolog.RUNTIME;
    return ok;
  }
    
  final void do_hset(int k1,int k2,int val,HeapStack heap)  throws PrologException {
    prolog.timeStamp=prolog.BBOARDTIME;
    if(Defs.isATOMIC(val)) 
      hset(k1,k2,val);
    else {
      hset_ref(k1,k2,heap.encodedCopy(val));
    }
    prolog.timeStamp=prolog.RUNTIME;
  }
  
  final int do_hget(int k1,int k2,HeapStack heap)  throws PrologException {
    int t=hget(k1,k2);
    if(prolog.G_ref==t) t=heap.decodedCopy((EncodedTerm)hget_ref(k1,k2));
    return t;
  }
  
  final void do_hremove(int k1,int k2)  throws PrologException {
     prolog.timeStamp=prolog.BBOARDTIME;
     hremove(k1,k2);
     prolog.timeStamp=prolog.RUNTIME;
  }
  
  final boolean do_isEmpty(int val) {
    return val==prolog.G_empty;
  }

  /*
  public IntStack hlist(int stamp) {
	  IntStack S=new IntStack();
    for (int i = 0; i < dictMax; i++) {
      DictEntry e=entries[i];
		  if (e!= null && e.timeStamp == stamp) {
		    S.push(e.pred); // not prolog ints !!!
		    S.push(e.fun);
		  }
		}
		return S;
  }
  */
  
}  // End class Dict

  /**
   * A dictionary entry.
   */
class DictEntry implements Stateful {
  final int pred;
  final int fun;
  int val; // only this can be updated
  final int timeStamp;
  final Prolog prolog;
  
  DictEntry(int pred,int fun,int val,Prolog prolog) {
    this.pred=pred;
    this.fun=fun;
    this.val=val;
    this.timeStamp=prolog.timeStamp;
    this.prolog=prolog;
  }
  
  DictEntry(DictEntry entry) {
    this.pred=entry.pred;
    this.fun=entry.fun;
    this.val=entry.val;
    this.timeStamp=entry.timeStamp;
    this.prolog=entry.prolog;
  }
  
  protected void xsetVal(int val) {
    this.val=val;
  }
  
  protected void xsetRef(Object oref) {
  }
  
  /** Do the specified keys match this entry? */
  final boolean isFound(int pred, int fun) {
    return (this.pred == pred && this.fun == fun);
  }

  //private final void setTimeStamp(int timestamp) {
  //  this.timeStamp=timestamp;
  //}
  
  //protected DictEntry makeNew() {
  //  return new DictEntry(pred,fun,val);
  //}
  
  //final DictEntry toClone() {
  //  DictEntry e=makeNew();
  //  e.setTimeStamp(this.timeStamp);
  //  return e;
  //}
  
  public String toString() {
    String s="<"+pred+","+fun+">-->"+val+"("+timeStamp+")";
    s="<"+prolog.codeStore.NAME(pred)+"/"+prolog.codeStore.GETARITY(pred)+","+
      prolog.codeStore.NAME(fun)+"/"+prolog.codeStore.GETARITY(fun)+">"+
      "==>" + val + ":("+timeStamp+")";
    return s;
  }
}// End class DictEntry

  /**
   * A dictionary entry. Inner class.
   */
final class RefDictEntry extends DictEntry {
  RefDictEntry(int pred,int fun, Object oref,Prolog prolog) {
    super(pred,fun,0,prolog);
    xsetRef(oref);
  }
  
  RefDictEntry(DictEntry entry,Object oref) {
    super(entry);
    xsetRef(oref);
  }
    
  Object oref; 
  
  protected void xsetVal(int val) {
    super.xsetVal(val);
    oref=null;
  }
  
  protected void xsetRef(Object oref) {
    super.xsetVal(prolog.G_ref);
    this.oref=oref;
  }
  
  //protected DictEntry makeNew() {
  //  return new RefDictEntry(super.makeNew(),oref);
  //}
  
  public String toString() {
    return super.toString()+"@"+oref;
  }
  
}// End class RefDictEntry


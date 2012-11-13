package prolog.logic;
/**
* Implements a symbol table as a hash-table. String values are mapped to integer atom ids.
* The current implementation uses
* linear probing.
*/
public final class AtomTable extends ObjectDict {

/** Provides access to the current Prolog and Machine */
final private Prolog prolog;

/**
* Create a new atom table.
* @param prolog The prolog.kernel instance with which this atom table is associated.
* @param atomMax The required size of the atom table.
*/
AtomTable(Prolog prolog, int atomMax) {
  super();
	this.prolog = prolog;
}

/**
* Create, or retrieve the atom id. for the specified atom name.
* @param s The specified atom name.
* returns The atom id.
*/
final int getAtom(String s) {
  return addObject(s);
}

synchronized final int addObject(Object O) {
    //if(null==O) return 0;
    Entry e=getEntry(O);
    if(null==e) e=addNewEntry(O,prolog.getMark());
    return e.ordinal+1;
}

synchronized final boolean removeObject(Object O) {
    return null!=remove(O);
  }
    
/**
* Retrieve the atom name associated with the given atomic or functor term.
* @param atomTerm The specified term.
* This is in the packed form &lt;FUNTAG&gt;&lt;atom Id.&gt;&lt;arity&gt;.
* returns The atom name.
* @throws ResourceException if the atom table is full.
*/
final String getAtomName(int atomTerm)  {
	Object O=getSymObject(atomTerm);
	//if(null==O) return Prolog.S_null; // do not do this!!!
	return (String)O;
}

final Object getSymObject(int atomTerm)  {
	return i2o(Defs.GETSYMNO(atomTerm));
  // might be null !!!
}

  /*
   implements portable hashkey for persistent uses - BinProlog compatible
   */
  
  public final int hkey0(int t) {
   
    if(Defs.isVAR(t)) return -1;
    
    int k;
    if(Defs.isIDENTIFIER(t)) {
      String s=getAtomName(t);
      k=string_hash(s,Defs.GETARITY(t));
    }
    else
      k=Defs.OUTPUT_INT(t);
    k=Math.abs(k);
    k=k&((1<<29)-1);
    return k;
  }


  public final int hkey(int t) {
    return Defs.INPUT_INT(hkey0(t));
  }

  public final static int string_hash(String s,int k) {
    int p=0;
    while(p<s.length()) k+=(k<<4)+s.charAt(p++);
    return k;
  }
  
  synchronized public final int o2i(Object O) {
    if(null==O) return 0;
    Entry e=getEntry(O);
    if(null==e) return 0;
    return e.ordinal+1;
  }
  
  synchronized public final Object i2o(int i) {
    i--;
    if(i<0 || i>getTop()) return null;
    Entry e=at(i);
    if(null==e) return null;
    return at(i).getKey();
  }
    
  
/**
* Create a new functor term with the specifed arity. 
* @param name The functor name.
* @param arity The arity of the functor (Atomic terms have zero arity).
* returns The term id in the form &lt;FUNTAG&gt;&lt;atom Id.&gt;&lt;arity&gt;.
* @throws ResourceException if the atom table is full.
*/
public final int newFunctor(String name, int arity) throws TypeException,ResourceException {
    if (arity > Defs.MAXARITY || null==name) {
		  throw new TypeException("Arity limit exceeded or bad name, name=" + name + ", arity=" + arity);
    }
    //return Defs.PUTARITY(Defs.PUTSYMNO(Defs.FUNTAG, getAtom(name)), arity);
    return Defs.addArity(getAtom(name),arity);
}
/**
* Creates an integer term or a new functor depending on the format of <code>name</code>.
*/
final int inputTerm(String name, int arity) throws TypeException,ResourceException {
	try {
		int i = Integer.parseInt(name);
		return Defs.INPUT_INT(i);
	}
	catch (NumberFormatException e) {
		return newFunctor(name, arity);
	}
}

/**
* Removes entries from the atom table which were created with a later epoch
* (time stamp) that the one specified.
*<p>ASSERT No new entries are created when an epoch is revoked until after this method is called.
* Otherwise entries will become orphaned due to gaps in the linear probing.
* @param timeStamp Specified epoch.
*/
void rollback(byte stamp) {
  Object[] keys=toKeys();
  for(int i=0;i<keys.length;i++) {
    Object O=keys[i];
    if(null==O) continue;
    byte ostamp=(byte)(((Integer)get(O)).intValue());
	  if (ostamp > stamp) {
	    remove(O);
	  }
  }
}
/**
* Dump all atom table entries.
*/
void dump() {
  if (Prolog.DEBUG) {
		Prolog.dump(toString());
	}	
}       

void dump_last() {
  byte stamp=prolog.LOADTIME;
    Prolog.dump("NEW SYMBOLS AT RUNTIME:");
    Prolog.dump(info()+"\n");
    //Prolog.dump(toString());
    Object[] keys=toKeys();
    for(int i=0;i<keys.length;i++) {
      Object O=keys[i];
      if(null==O) continue;
      byte ostamp=(byte)(((Integer)get(O)).intValue());
      if (ostamp > stamp) {
        Prolog.dump("<"+O+">");
      }
    }
}


} // end class AtomTable


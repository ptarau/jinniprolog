package prolog.logic;

/**
Fast and portable hash table implementation that provides
implicit symbol table functionality by associating a unique
integer to each key. This provides chronological order when
only additions are made to the table.
 */
public class ObjectDict implements Stateful {

  public static final int MINSIZE=4;
   
  public ObjectDict(int size) {
    init(size);
  }
    
  public ObjectDict() {
    this(0);
  }
  
  /**
     Replaces all fields with the other ObjectDoct's fields
   */
  public void cloneFrom(ObjectDict OD) { 
    entries=OD.entries;
    count=OD.count;
  
    syms=OD.syms;
    top=OD.top;

    frees=OD.frees;
    ftop=OD.ftop;
  
    getCount=OD.getCount;
    addCount=OD.addCount;
    updateCount=OD.updateCount;
    removeCount=OD.removeCount;
  }
  
  public void clear() {
    init(0);
  }
    
  transient private Entry[] entries;
  transient private int count;
  
  transient private Entry[] syms;
  transient private int top;

  transient private int[] frees;
  transient private int ftop;
  
  transient private int getCount;
  transient private int addCount;
  transient private int updateCount;
  transient private int removeCount;
  
  
  private final void init(int size) {
    if(size<=0) size=MINSIZE;
    else if(!((size & -size) == size)) {
      int l = MINSIZE;
      while(l<size) l<<=1;
      size=l;
    };
    
    entries=new Entry[size];
    count=0;
    
    syms=new Entry[size];
    top=-1;
    
    frees=new int[size];
    ftop=-1;
    
    addCount=0;
    getCount=0;
    updateCount=0;
    removeCount=0;
  }

  public int[] getCounts() {
    int[] counts={addCount,getCount,updateCount,removeCount};
    return counts;
  }
  
  public final Entry getEntry(Object k) {
    getCount++;
    if(null==k) return null;
    int h=Entry.o2hash(k);
    int i=Entry.indexFor(h,entries.length);
    return getEntry(i,h,k);
  }
    
  private final Entry getEntry(int i,int h,Object k) {
    Entry e = entries[i];
    while(null!=e) {
      if(e.eq(h,k)) return e;
      e=e.next;
    }
    return null;
  }
  
  /**
      Gets the object associayed to a key.
   */
  public Object get(Object key) {
    Entry e=getEntry(key);
    if(null==e) return null;
    return e.value;
  }

  public final int getOrdinal(Object key) {
    Entry e=getEntry(key);
    if(null==e) return -1;
    return e.ordinal;
  }
  /**
     Associates an object to a key.
   */
  public Object put(Object k,Object v) {
    if(null==k || null==v) return null;
    int h=Entry.o2hash(k);
    int i=Entry.indexFor(h,entries.length);
    Entry e=getEntry(i,h,k);
    if(null!=e) {
      updateCount++;
      Object o=e.value;
      e.value=v;
      return o;
    }  
    makeEntry(i,k,v);
    return null;     
  }
 
  /**
     Allows to use ObjectDict as a Tree data structure.
     This operation can be seen as adding, unless it already exists,
     a subtree named by a key to this ObjectDict.
     This simple idea ports easily to all Maps.
  */
  public ObjectDict child(Object k) {
    ObjectDict child=(ObjectDict)get(k);
    if(null==child) {
      child=new ObjectDict();
      put(k,child);
    }
    return child;
  }
  
  public void removeAll() {
    Object Ks[]=toKeys();
    for(int i=0; i<Ks.length;i++) {
      Object K=Ks[i];
      Object V=remove(K);
      if(V instanceof ObjectDict) {
        ((ObjectDict)V).removeAll();
      }
    }
  }

  /*
  public void expellFrom(Prolog P) {
    Object Ks[]=toKeys();
    for(int i=0; i<Ks.length;i++) {
      Object K=Ks[i];
      Object V=remove(K);
      P.atomTable.removeObject(this);
      if(V instanceof ObjectDict) {
        ((ObjectDict)V).expellFrom(P);
      }
    }
  }
  */
  
  public final Entry addNewEntry(Object k,Object v) {
    int h=Entry.o2hash(k);
    int i=Entry.indexFor(h,entries.length);
    return makeEntry(i,k,v);
  }
   
  private final Entry makeEntry(int i,Object k,Object v) {
    Entry e=new Entry(k,v,entries[i]);
    e.ordinal=addSym(e);
    entries[i]=e;
    addCount++;
    count++;
    expandHash();
    return e;   
  }
  
  /**
     Removes the association between a key and a value
     from this ObjectDict.
   */
  public Object remove(Object k) {
    removeCount++;
    if(null==k) return null;
    int h=Entry.o2hash(k);
    int i=Entry.indexFor(h, entries.length);

    Entry prev = entries[i];
    Entry e = prev;

    while (e != null) {
      Entry next = e.next;
      if(e.eq(h,k)) {
        count--;
        if (prev == e) 
          entries[i] = next;
        else
          prev.next = next;
        break;
      }
      prev = e;
      e = next;
    }
    if(null==e) return null;
    Object val=e.value;
    delSym(e);
    shrinkHash();
    return val;
  }
  
  /**
    combined with add, this can be used safely
    as a stack - this allows chronological
    undo of latest additions - it should be useful
    for recovering syms on backtracking
  */  
  public Object pop() {
    if(isEmpty()||count!=top+1) return null; // not safe to use as a stack
    return remove(syms[top]);
  }
    
  private final int addSym(Entry e) {
    while(ftop>=0) {
      int i=frees[ftop--];
      if(i<=top) {
        syms[i]=e;
        return i;
      }
    };  
    syms[++top]=e;
    return top;
  }
  
  private final void delSym(Entry e) {
    syms[e.ordinal]=null;
    while(top>-1 && null==syms[top]) --top;
    if(e.ordinal<=top) frees[++ftop]=e.ordinal;
    e.clear();
  }
 
  public final Entry at(int i) {
    return syms[i];
  }
   

  private final void resize(int newCapacity) {
    Entry[] newsyms=new Entry[newCapacity];
    System.arraycopy(syms,0,newsyms,0,top+1);
    syms=newsyms;
    
    int[] newfrees=new int[newCapacity];
    System.arraycopy(frees,0,newfrees,0,ftop+1);
    frees=newfrees;
    
    entries = new Entry[newCapacity];
    for (int j = 0; j <= top; j++) {
      Entry e = syms[j];
      if (e != null) {
        int i = e.getIndex(newCapacity);  
        e.next=entries[i];
        entries[i] = e;
      }
    }
  }

  private final void expandHash() {
    if(top+1>=entries.length) 
      resize(entries.length<<1);
  }
  
  private final void shrinkHash() {
    if(entries.length>MINSIZE && top+1<entries.length>>2)
      resize(entries.length>>1);
  }
  
  public final int size() {return count;}
  
  public final boolean isEmpty() {
    return size()>0;
  }
  
  public final int capacity() {
    return entries.length;
  }
  
  public final int getTop() {
    return top;
  }
  
  public Object[] toKeys() {
    Object[] es=new Object[top+1];
    for(int i=0;i<=top;i++) {
      Entry e=syms[i];
      if(null!=e) es[i]=e.getKey();
    }
    return es;
  }
  
  public Object[] toValues() {
    Object[] es=new Object[top+1];
    for(int i=0;i<=top;i++) {
      Entry e=syms[i];
      if(null!=e) es[i]=e.value;
    }
    return es;
  }
  
  public void compact() {
    while(ftop>=0) {
      int i=frees[ftop--];
      Entry e=syms[i];
      if(null!=e) Interact.errmes("error compacting dict at:"+ftop,new SystemException());
      e=syms[top];
      syms[top--]=null;
      e.ordinal=i;
      syms[i]=e;
    }
    Prolog.dump("info after compact\n"+info());
  }
   
  public void shuffle(int seed) {
    compact();
    
    /*    
    for(int i=0;i<top+1;i++) {
      Entry e=syms[i];
      Prolog.dump(i+"=>HERE: "+e);
    }
    */
    
    Tools.shuffle(seed,syms,top+1);
    
    /*
    for(int i=0;i<top+1;i++) {
      Entry e=syms[i];
      Prolog.dump(i+"=>THERE: "+e);
    }
    */
    
    for(int i=0;i<top+1;i++) {
      Entry e=syms[i];
        Prolog.dump(e+"=e,i="+i);
      e.ordinal=i;
    }
    
  }
   
  public ObjectIterator getKeys() {
    return new ObjectIterator(syms,top);
  }
  
  public ObjectDict intersect_with(ObjectDict other) {
    ObjectIterator I=getKeys();
    ObjectDict D=new ObjectDict();
    while(I.hasNext()) {
      Object K=I.next();
      Object otherval=other.get(K);
      if(null==otherval) continue;
      Object val=get(K);
      D.put(K,val);
    }
    return D;
  }
  
  public ObjectDict difference_with(ObjectDict other) {
    return difference_with(other,new ObjectDict());
  }
  
  public ObjectDict difference_with(ObjectDict other,ObjectDict D) {
    ObjectIterator I=getKeys();
    while(I.hasNext()) {
      Object K=I.next();
      Object otherval=other.get(K);
      if(null!=otherval) continue;
      Object val=get(K);
      D.put(K,val);
    }
    return D;
  }
  
  public ObjectDict simdif_with(ObjectDict other) {
     ObjectDict D=difference_with(other);
     return other.difference_with(this,D);
  }
  
  public int intersect_count(ObjectDict other) {
    ObjectIterator I=getKeys();
    int ctr=0;
    while(I.hasNext()) {
      Object val=other.get(I.next());
      if(null==val) continue;
      ++ctr;
    }
    return ctr;
  }
  
  public int simdif_count(ObjectDict other) {
    return this.size()+other.size()-2*intersect_count(other);
  }
  
  public int dif_count(ObjectDict other) {
    return this.size()-intersect_count(other);
  }
  
  public int union_count(ObjectDict other) {
    return this.size()+other.size()-intersect_count(other);
  }
  
  public String info() {
    return "{count="+count+
      ", entries="+entries.length+
      ", top="+top+
      ", ftop="+ftop+
      ", get="+getCount+
      ", add="+addCount+
      ", update="+updateCount+
      ", remove="+removeCount+
      "}";
  }
  
  public String toString() {
    StringBuffer buf=new StringBuffer("[");
    int i=-1;
    boolean first=true;
    while(i<top) {
      Entry e=syms[++i];
      if(null!=e && !first) buf.append(",");
      if(null!=e) {
        first=false;
        buf.append("["+(i)+"]:");
        buf.append(e);
      }
    }
    buf.append("]");
    return buf.toString();
  }
  
  private void writeObject(java.io.ObjectOutputStream s)
        throws java.io.IOException {
    s.defaultWriteObject();
    s.writeInt(count);
    int i=-1;
    while(i<top) {
      Entry e=syms[++i];
      if(null==e) continue;
      s.writeObject(e.getKey());
      s.writeObject(e.value);
    }  
  }
  
  private void readObject(java.io.ObjectInputStream s) 
       throws java.io.IOException, ClassNotFoundException {
    s.defaultReadObject();
    int count = s.readInt();
    init(count);
    for (int i=0; i<count; i++) {
      Object key = s.readObject();
      Object value = s.readObject();
      addNewEntry(key,value);
    }
  }
}


package prolog.logic;

/**
Provides iteration over an ObjectDict.
 */
public class ObjectIterator implements Stateful {

  public ObjectIterator(Entry[] syms,int top) {
    this.syms=syms;
    this.top=top;
    this.index = 0;
    scroll();
  }

  private int index; 
  private Entry[] syms;
  private int top;
  
  /** Check if it has a next element.
   */
  public final boolean hasNext() {
    return index<=top && syms[index]!=null;
  }

  private final void scroll() {
    while(index <= top && syms[index] == null) index++;
  }
  
  /**
  Returns the next element.
   */
  public final Entry nextEntry() { 
    Entry e=syms[index++];
    scroll();
    return e;
  }
  
  /**
   Returns the next element.
   */
  public final Object next() { 
    return nextEntry().getKey();
  }
}
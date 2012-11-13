package prolog.logic;

/**
   Implements an Entry in the ObjectDict hash based dictionary
 */
public class Entry implements Stateful {
  public int ordinal;
  private final Object key;
  public Object value;
  private final int code;
  Entry next;
 
public Entry(Object key,Object value,Entry next) {
    this.key=key;
    this.code=o2hash(key);
    this.value=value;
    this.next=next;
    this.ordinal=-1;
  }
  
public Object getKey() {
    return key;
  }
  
public void clear() {
    this.value=null;
    this.next=null;
  }
   
  public final int hashCode() {
    return code;
  }

  final int getIndex(int len) {
    return indexFor(code,len);
  }

  final static int indexFor(int h, int length) {
    return h & (length-1);
  }  
  
  final boolean eq(int code,Object key) {
    return this.code==code && this.key.equals(key);
  }
  
  final static int o2hash(Object x) {
    int h = x.hashCode();
    
     h += ~(h << 9);
     h ^=  (h >>> 14);
     h +=  (h << 4);
     h ^=  (h >>> 10);
        
    return h;
  }
    
  public String toString() {
    return //ordinal+":"+
      key+"="+value;
  }
}



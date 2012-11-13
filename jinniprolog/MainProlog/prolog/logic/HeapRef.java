package prolog.logic;

/**
 * Implements a wrapper for heap references as
 * Java objects
 */
public class HeapRef implements Stateful {
  public HeapRef(int ref) {
    this.ref=ref;
  }

  final private int ref;
  
  final public int getRef() {
    return ref;
  }
  
  public boolean equals(Object O) {
    return O instanceof HeapRef && ((HeapRef)O).ref==ref;
  }
  
  public int hashCode() {
    return ref;
  }
   
  public String toString() {
    return "'&"+ref+"'";
  }
}

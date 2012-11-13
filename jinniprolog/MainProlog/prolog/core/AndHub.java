package prolog.core;
import prolog.kernel.*;
import prolog.logic.*;

/**
   An AndHub is a generic AND synchronizer.
   An array of exactly max slots is first filled up by Producers
   such that each slot holds exactly one value. Producers are blocked until 
   all values are ready or if they try to reassign a cell that is full.
   Consumers are blocked until the array is full or if they try
   to take a value more than once.
   A typical use is as a dataflow primitive for a max argument function.
   For this purpose a getAll operation is provided that consumes 
   all values atomically, when ready.   
*/

public class AndHub implements Stateful {
  private static final Object empty=new Object();
  
  public AndHub(int max) {
    this.dict=new Object[max];    
    for(int i=0; i<max; i++) {
      dict[i]=empty;
    }
    this.max=max;
    initCounts();
  }
   
  private Object[] dict;
  private final int max;
  private int putCount;
  private int getCount;
  private int addCount;
  private int removeCount;
  
  private void initCounts() {
    this.putCount=0;
    this.getCount=0;
    this.addCount=0;
    this.removeCount=0;
  }
  
  /*
  private void t(String s) {
    //System.err.println("!! "+s+",p="+putCount+",g="+getCount+"a="+addCount+",r="+removeCount);
  }
  */
  public void add(Object val) {
    set(this.addCount++,val);
  }
  
  public synchronized void set(int key,Object val) {
    //t(">set:"+key+":"+val);
    while(putCount==max || empty!=dict[key]) {
      // wait for others but only set once
      try {
        wait();
      }
      catch(InterruptedException e) {
      }
    }
    dict[key]=val; // single final put operation
    putCount++; 
    //t("<set:"+key+":"+val);
    notifyAll();
  }
  
  public Object remove() {
    return get(this.removeCount++);
  }
  
  // wait for others but only get once
  public synchronized Object get(int key) {  
    //t(">get:"+key); 
    while(putCount<max || empty==dict[key]) {
      try {
        wait();
      }
      catch(InterruptedException e) {
      }
    }
    Object result=dict[key];
    dict[key]=empty; // ensures each result is consumed exactly once
    getCount++;
    //t("<get:"+key); 
    if(getCount==max) {
      initCounts();
    }
    notifyAll();
    return result;
  }
  
  public synchronized Object[] getAll() {   
    while(putCount<max || getCount>0) {
      try {
        wait();
      }
      catch(InterruptedException e) {
      }
    }
    Object[] result=new Object[max];
    for(int i=0;i<max;i++) {
      result[i]=dict[i];
      dict[i]=empty;
    }
    initCounts();
    notifyAll();
    return result;
  }
  
  
  public Fun collectAll() {
    return new Fun("all",getAll());
  }
  
  public void stop() {
    this.dict=null;
  }
  
  public String toString() {
    StringBuffer buf=new StringBuffer();
    for(int i=0;i<max;i++) {
      if(i>0) buf.append(",");
      Object x=dict[i];
      buf.append((empty==x)?"?":x.toString());
    }
    return "{"+putCount+"-"+getCount+"+"+addCount+"-"+removeCount+"}:["+buf+"]";
  }
 
}

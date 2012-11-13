package prolog.kernel;
import prolog.logic.*;

public class SimpleLock {
   
  private static ObjectDict locks=new ObjectDict();
  
  synchronized public static SimpleLock theLock(Object lockName) {
    SimpleLock lock=(SimpleLock)locks.get(lockName);
    if(null==lock) {
      lock=new SimpleLock();
      locks.put(lockName,lock);
    }
    return lock;
  }
  
  public static int enter(Object lockName) {
    return theLock(lockName).lock();
  }
  
  public static void exit(Object lockName) {
    theLock(lockName).unlock();
  }
  
  public SimpleLock() {
  }
  
  private boolean locked=false;
  
  synchronized public int lock() {
    int theOne=1;
    while(locked) {
      try {
        theOne=0;
        wait();
      }
      catch(InterruptedException e) {
      }
    }
    if(theOne>0) locked=true;
    notifyAll();
    return theOne;
  }
  
  synchronized public void unlock() {
    notifyAll();
    locked=false;
  }
}
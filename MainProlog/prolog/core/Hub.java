package prolog.core;
import prolog.kernel.*;
import prolog.logic.*;

/**
 * An Hub is a device which synchronizes
 * Producers and Consumers. Called through Reflection from Prolog.
 */
public class Hub implements Stateful {
  private static final long secTimout=10;
  private static long msTimeout=1000L*secTimout;  // secs max
  private static Hub atomizer=new Hub(msTimeout);
  public static boolean trace=true;
  
  public static void reset_critical(long secs) {
    if(null!=atomizer) atomizer.stop();
    msTimeout=1000L*secs;
    atomizer=new Hub(msTimeout);
  }
  
  public static int enter_critical() {
    return enter_critical(Thread.currentThread());
  }
  
  public static int enter_critical(Object who) {
    int ok=atomizer.putElement(who);
    if(0==ok) {
      Object frozen=atomizer.port;
      reset_critical(secTimout);
      PrologException e=
        new PrologException("timeout entering critical for: "+who+",frozen:"+frozen);
      if(trace) {
        e.printStackTrace(); ////
        //Interact.fatal_error("???"+e);
      }
      else Interact.errmes("atomizer_error",e);
    }
    return ok;
  }
  
  public static int exit_critical() {
    return exit_critical(Thread.currentThread());
  }
  
  public static int exit_critical(Object who) {
    int ok=0;
    Object other=null;
    if(atomizer.contains(who)) {
      other=atomizer.collect();
      if(who.equals(other)) ok=1;
    }
    if(0==ok) {
      reset_critical(secTimout);
      PrologException e=
        new PrologException("error exiting critical, expecting: "+who+",found: "+other);
      if(trace) {
        e.printStackTrace(); ////
        //Interact.fatal_error("???"+e);
      }
      else Interact.errmes("atomizer_error",e);
    }
    return ok;
  }
  
  public Hub(long t) {
    this.forever=(0L==t);
    this.port=null;
    this.timeout=t+System.currentTimeMillis() ;
  }
  
  private final boolean forever;
  private Object port;
  private long timeout;
  
  synchronized public boolean contains(Object other) {
    return other.equals(port);
  }
  
  public synchronized Object collect() {
    int ok=1;
    while(null==port) {
      try {
        if(forever) wait();
        else {
          long t=this.timeout-System.currentTimeMillis();
          if(t<=0) {ok=0; break;} // avoid: wait(0) - it means forever !!!
          wait(t);
        }
      }
      catch(InterruptedException e) {
      }
    }
    //Prolog.dump("get port:"+timeout+port);
    Object result=null;
    if(ok>0) {
      result=port;
      port=null;
    }
    notifyAll();
    return result;
  }
  
  public synchronized int putElement(Object T) {
    int ok=1;
    while(null!=port) {
      try {
        if(forever) wait();
        else {
          long t=this.timeout-System.currentTimeMillis();
          //Prolog.dump("putElement waiting :"+t+" for: "+port);
          if(t<=0) {ok=0; break;}; // avoid: wait(0) - it means forever !!!
          wait(t);
        }
      }
      catch(InterruptedException e) {
      }
    }
    if(ok>0) port=T;
    //Prolog.dump("put port:"+port);
    notifyAll();
    return ok;
  }
  
  synchronized public void stop() {
    this.timeout=-1L;
  }

}

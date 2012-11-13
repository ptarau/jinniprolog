package prolog.core;
import prolog.kernel.*;
import prolog.logic.*;
/**
   An Barrier ensures a number of threads wait jointly. When all finish,
   a Runnable action is executed. The barrier is usable only once and
   is expected to be garbage collected after it accomplishes its mission.
*/

public class Barrier implements Stateful {
  
  public Barrier(int count,Runnable action) {
    this.count=count;
    this.action=action;
  }
  
  public Barrier(int count) {
    this(count,null);
  }
   
  private Runnable action;
  private int count;
  
  public void await() {
    this.count--;
    await0();
  }
  
  synchronized private void await0() {
    while(count>0) {
      try {
        wait();
      }
      catch(InterruptedException e) {
      }
    }
    if(null!=this.action) {
      Runnable a=this.action;
      this.action=null;
      a.run();
    }  
    notifyAll();
  }
  
  public void stop() {
    this.count=0;
  }
  
  public String toString() {
    return super.toString()+"=>count="+count;
  }
}

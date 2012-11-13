package agentgui;

import prolog.logic.*;
import prolog.kernel.*;
import prolog.core.*;

public class PrologAdaptor implements Runnable {
	private Prolog prolog;
    private final Object metacall;
    
	public PrologAdaptor() {
		this(null);
	}

    public PrologAdaptor(Object metacall) {
      this.prolog = Prolog.getDefaultProlog();
      this.metacall=metacall;
    }
    
	public Object callProlog(Object query) {
	   return callProlog(query,null,null);
	}
	
	public Object callProlog(Object query,PrologReader input,PrologWriter output) {
		Machine E = Top.new_machine(this.prolog,input,output); // possibly cached
		Object R = E.query_engine(query); // does work on server
		// System.err.println(E.hashCode() +":"+query+"=>"+R);
		return R;
	}
	
   public void run() {	
      if(null==this.metacall)
	    System.out.println("override run in prologAdaptor or define metacall");
      else {
        callProlog(this.metacall);
      }
   }
	
   public void bg_run() {
	   Thread T=new Thread(this,"PrologAdaptorThread");
	   T.setDaemon(true);
	   T.start();
   }
}

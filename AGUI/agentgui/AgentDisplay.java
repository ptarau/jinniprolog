package agentgui;

import java.awt.Container;
import prolog.logic.*;
import prolog.kernel.*;
import prolog.core.*;
import jgui.*;

public class AgentDisplay extends PrologAdaptor {
    public static int instance=0;
    
	private VFrame vframe;
	private InnerFrame frame;
	private String port;
	private int x,y,dx,dy;
	private int age;
	
	public AgentDisplay(VFrame vframe) {
	   this(vframe,"vDisplay"+ (++instance),600+10*instance,10*instance,400,400);
	}
	
	public AgentDisplay(VFrame vframe,String port,int x,int y,int dx,int dy) {
		this.vframe=vframe;
		this.port=port;
		this.x=x;
		this.y=y;
		this.dx=dx;
		this.dy=dy;
		this.age=-1;
		this.frame=null;
	}

	public void create() {
      this.frame=new InnerFrame(port,x,y,dx,dy);
      vframe.addInnerFrame(frame);
      
      /*
      Fun goSwing=new Fun("set_global_prop","gui","swing");
      callProlog(goSwing);
      Fun startIde=new Fun("new_ide",frame,"true");
      callProlog(startIde);
      */
      
      Displayer displayer=new Displayer(400,400);
      //frame.add(displayer); //#
      frame.getContentPane().add(displayer);
      frame.addComponentListener(displayer);
      
      displayer.append_text("<html><i>starting</i> <b>PORT:"+port+"<b></html>");
      PrologWriter output=new VWriter(displayer); 
      
      // call RLI directly
      Fun rliServer=new Fun("rli_start_server",port,output);
      //IOAdaptor ioAdaptor=new IOAdaptor(displayer);
      
      //System.out.println("!!!HERE");
      
      callProlog(rliServer,null,output);
      
      /*
      // this does not work because rli_call passes a fresh instance

      Fun setHandle=new Fun("<=","displayer",displayer);
      callProlog(setHandle);
      
      Fun rliCall=new Fun("rli_call",port,setHandle);
      
      callProlog(rliCall);
      */

	}
	
	public void run() {
	  ++age;
	  if(0==age) {
	    create();
	  } 
	  else if(1== age % 2) { // odd
	    frame.setVisible(false);
	  }
	  else { // age>0 and even
	    frame.setVisible(true);   
	  }
         
   }
}

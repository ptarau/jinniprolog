package agentgui;

import java.awt.Container;
import prolog.logic.*;
import prolog.kernel.*;
import prolog.core.*;

public class PrologIDE extends PrologAdaptor {
	public String vPort="vProlog";
	public String vName;
	public static int instCount=0;
	
	private VFrame vframe;
	private String ide;
	
	public PrologIDE(VFrame vframe,String vName,String ide) {
		super();
		this.vframe=vframe;
		this.vName=vName;
		this.ide=ide;
	}

	/*
	public void oldrun() {	
		 InnerFrame frame=new InnerFrame("PrologAdaptor",500,0,400,300);
		 vframe.addInnerFrame(frame);
		 Fun goSwing=new Fun("set_global_prop","gui","swing");
		 callProlog(goSwing);
		 //new_console(Container,Query,Rows,Cols,Console)
		 Fun startIde=new Fun("new_console",frame,"true",10,16,new Var(0));
		 callProlog(startIde);
   }
   */
	
	public String newName() {
	  if(0==instCount) 
		return  vName;
	  else
		return vName+instCount;
	}
	
	public String newPort() {
		  if(0==instCount) 
			return  vName;
		  else
			return vName+instCount;
   }
	
	public void run() {	
		 Fun goSwing=new Fun("set_global_prop","gui","swing");
		 Fun goVirtual=new Fun("set_global_prop","desktop","virtual");
		 callProlog(goSwing);
		 callProlog(goVirtual);
		 // temporary fix until ide is fully self adjusting
		 
		 if("new_ide".equals(ide)) {
			InnerFrame.def_x=520;
			InnerFrame.def_y=680;
		 }
		 else {
			InnerFrame.def_x=480;
			InnerFrame.def_y=300;  
		 }
			 
		 //System.err.println(ide+" x="+InnerFrame.def_x+",y="+InnerFrame.def_y);
		 
		 Fun startIde=new Fun("run_rli_gui",ide,newName(),newPort(),"true");
		 /*
		 if("new_ide".equals(ide)) {
			 InnerFrame.def_x=dx;
			 InnerFrame.def_y=dy;
		 }
		 */
		 ++instCount;
		 callProlog(startIde);
   }
}

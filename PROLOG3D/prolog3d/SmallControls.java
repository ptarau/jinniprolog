package prolog3d;

import java.awt.*;
import java.awt.event.*;
import prolog.logic.*;
import prolog.kernel.*; //?!

/**
   Adds a control Panel to the 3D display
 */
public class SmallControls extends Controls {
  public SmallControls(Prolog3D M,LayoutEngine LG) {
    super(M,LG);
  }
  
  public void makeControls() {
    setBackground(new Color(50,50,100));
    setForeground(new Color(255,255,255));
    makeOutput("Center",this);
    makeButtons("West",this);
  }
  
  private void makeOutput(String where,Panel P) {
    int scroll=TextArea.SCROLLBARS_VERTICAL_ONLY;
    //int scroll=TextArea.SCROLLBARS_NONE;
    this.output=new TextArea("",4,24,scroll);
    this.output.setBackground(new Color(50,50,100));
    this.output.setForeground(new Color(255,255,255));
    this.output.setFont(new Font("Helvetica",Font.PLAIN,16));
    this.output.setEditable(false);
    P.add(where,this.output);
  }
  
  private void makeButtons(String where,Panel P) {
    int bcount=3;
    if(null!=LG) bcount++;
    
    Panel B=new Panel(new GridLayout(bcount,1));
    B.setBackground(new Color(50,50,100));
    B.setForeground(new Color(0,0,0));
    
    Button IDE=new Button("Run");
    IDE.addActionListener(this);
    B.add(IDE);

    Button Print=new Button("Print");
    Print.addActionListener(this);
    B.add(Print);
    
    if(null!=LG) { 
      Button Freeze=new Button("Freeze");
      Freeze.addActionListener(this);
      B.add(Freeze);
    }
    
    Button Quit=new Button("Quit");
    Quit.addActionListener(this);
    B.add(Quit);
    
    P.add(where,B);
  }
  
  public void actionPerformed(ActionEvent e) {
    String c=e.getActionCommand();
    Prolog3D.pp("M="+M+" cmd="+c);
    if("Run".equals(c)) {
      callIDE();
    }
    else if("Print".equals(c)) M.printWorld(Params.printH,Params.printW);
    else if(null!=LG && "Freeze".equals(c)) Prolog3D.toFrozen(LG,400,400);
    else if("Quit".equals(c)) {
      if(null!=LG) LG.stop();
      if(null!=M) {
        M.stopWorld();
        M.exit();
      }  
    }
    else Prolog3D.pp("unknown event="+e);
  }

  public void callIDE() {
    Machine E=Top.new_machine();
    Fun goal=new Fun("call","ide");
    E.load_engine(goal);
    E.get_answer();
    //JinniButton new_button(Container C,String name,Engine M)
  }

  public void print(Object O) {
    output.setText(O.toString());
  }
}

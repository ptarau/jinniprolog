package jgui;
import prolog.core.PrologApplet;
import prolog.core.Hub;
import prolog.kernel.*;
import prolog.logic.*;

import javax.swing.*;
//import javax.swing.event.*;
//import javax.swing.text.*;
//import javax.swing.filechooser.*;

import java.awt.*;
import java.awt.event.*;
//import java.awt.image.*;

import java.applet.*;
import java.io.File;

/**
  Provides builtins for GUI programs.
  Called though Reflection from Prolog.
*/
public class Start implements Stateful {

  public static long thread_wait=10;
  
  public static void  set_thread_wait(long time) {
    thread_wait=time;
  }
  
  public static void main(String[] args) {
    System.out.println("start this from prolog");
  }

  //// ????
  public static final void invokeLater(Runnable R) {
    //System.err.println("invoking from Thread:"+Thread.currentThread());
    //prolog.kernel.Machine.sleep_ms(10);
    //Hub.enter_critical();
    //System.err.println("invoking Runnable:"+R);
    try {
      SwingUtilities.invokeLater(R);
    }
    catch(Exception e) {
      Interact.errmes("error in invokeLater=>"+R,e);
    }
    //(new Thread(R,"LaterThread")).start();
    //Hub.exit_critical();
    if(thread_wait>0) prolog.kernel.Machine.sleep_ms(thread_wait);
  }
  
  public static int defX=480;
  public static int defY=640;
  public static int defRows=12;
  public static int defCols=24;
  public static int gapX=2;
  public static int gapY=2;
  
  public static String defaultFontName="Default";
  public static int defaultFontSize=12;
  public static int defaultFontStyle=Font.PLAIN;
  public static Color defaultFgColor=null;
  public static Color defaultBgColor=null;

  private static Font defaultFont=
      new Font(defaultFontName,defaultFontSize,defaultFontSize);
  
  public static void setColors(JComponent C) {
    to_default_fg(C);
    to_default_bg(C);
  }

  public static void setFonts(JComponent C) {
    to_default_font(C);
  }

  public static void setLooks(JComponent C) {
    setFonts(C);
    setColors(C);
  }

  public Start() {
  }
  
  public static void stopComponent(JComponent C) {
    if(C instanceof JinniButton) {
      JinniButton B=(JinniButton)C;
      B.stop();
    }
  }
  
  /*
  public static Font getDefaultFont() {
    //Prolog.dump("df="+defaultFont);
    return defaultFont;
  }
  */

  
  public static void set_font_name(String name) {
    if(!name.equals(defaultFontName)) {
      defaultFontName=name;
      defaultFont=new Font(name,defaultFontStyle,defaultFontSize);
    }
  }
  
  public static void set_font_size(int size) {
    defaultFontSize=size;
    defaultFont=new Font(defaultFontName,defaultFontStyle,size);
  }
  
  public static void inc_font_size(int size) {
    size+=defaultFontSize;
    set_font_size(size);
  }
  
  public static void set_font_style(String s) {
    int style=defaultFontStyle;
    if("plain".equals(s)) style=Font.PLAIN;
    else if("bold".equals(s)) style=Font.BOLD;
    else if("italic".equals(s)) style=Font.ITALIC;    
    if(defaultFontStyle!=style) {
      defaultFontStyle=style;
      defaultFont=new Font(defaultFontName,style,defaultFontSize);
    }
  }
  
   
  public static void to_default_font(JComponent C) {
    C.setFont(defaultFont);
  }
    
  public static void to_default_fg(JComponent C) {
    if(null==defaultFgColor) return;
    C.setForeground(defaultFgColor);
  }

  public static void to_default_bg(JComponent C) {
    if(null==defaultBgColor) return;
    C.setBackground(defaultBgColor);
  }

  /*
  public static Color get_fg_color() {
    return defaultFgColor;
  }

  public static Color get_bg_color() {
    return defaultBgColor;
  }
  */

  public static void set_fg_color(double r,double g,double b) {
    defaultFgColor=new_color(r,g,b);
  }

  public static void set_bg_color(double r,double g,double b) {
    defaultBgColor=new_color(r,g,b);
  }

  public static LayoutManager to_layout(String name,int x,int y) {
    LayoutManager M=null;
    
    if(name.equals("grid")) {
      M=new GridLayout(x,y,gapX,gapY);
    }
    else if(name.equals("border")) {
      M=new BorderLayout();
    }
    else if(name.equals("card")) {
      M=new CardLayout();
    }
    else if(name.equals("flow")) {
      M=new FlowLayout();
    }
    else {
      JavaIO.warnmes("unknown layout: "+name);
      M=new FlowLayout();
    }
    return M;
  }
  
  ////
  synchronized public static JinniFrame new_frame(String title,String layout,
                                     int x,int y,int kind) {
    LayoutManager L=Start.to_layout(layout,x,y); // more work to decode grid - etc.
    JinniFrame F=new JinniFrame(title,L,kind);
    return F;
  }

  /**
    new_button(JinniContainer,Name,Action,Button): 
    creates a Button with label Name
    and attaches to it an action Action
  */
  synchronized public static JinniButton new_button(Container C,String name,Machine M) {
    JinniButton JB=new JinniButton(name,M);
    C.add(JB);
    return JB;
  }

  /**
  * new_label(JinniContainer,TextToBeDisplayed,Label): 
  * creates a label with centered text
  *
  */
  
  synchronized public static JLabel new_label(Container C,String name) {
    JLabel L=new JLabel(name);
    //L.setAlignment(JLabel.CENTER);
    C.add(L);
    return L;
  }

/*
  set_label: directly through Reflection
*/

  public static String new_file_dialog(int mode) {
    return new_file_dialog(mode,"pl");
  }
 
  synchronized public static String new_file_dialog(int mode,String filter) {
	try {
    JinniFrame C=new JinniFrame("File Dialog");
    
    JinniFileDialog D;
    /*
    if(0==mode) {
      D=new JinniFileDialog(C,"Load",JFileChooser.OPEN_DIALOG,filter);
    }
    else {
      D=new JinniFileDialog(C,"Save",JFileChooser.SAVE_DIALOG,filter);
    }
    */
    D=new JinniFileDialog(C,"Load/Save",mode,filter);
    //Prolog.dump("here");
    Start.setLooks(D);
    //D.show();
    //String fname=D.getSelectedFile().getName();
    String fname=D.getChoice();
    if(null==fname) return null;
    //String dname=fname; // $$ D.getSelectedDirectory();
    //if(null==dname) return null;
    //String result=dname+fname;
    
    //D.dispose();
    C.dispose();
    return fname;
	}
	catch(Exception e){
	  e.printStackTrace();	
	  return null;
	}
  }

  
  synchronized public static JinniPanel new_panel(Container C,String layout,int x,int y) {
    LayoutManager L=Start.to_layout(layout,x,y);
    JinniPanel P=new JinniPanel(L);
    C.add(P);
    //$$C.addComponentListener(P);
    C.setVisible(true);
    return P;
  }
  
  /** 
  new_text ARGS:
    1=Parent Container
    2=initial text content
    3=rows
    4=cols
    5=returned handles
  */
  
  synchronized public static JinniText new_text(Container C,String oldText,int rows,int cols) {
    JinniText T=new JinniText(oldText);
    if(rows>0 && cols>0) {
      T.setRows(rows);
      T.setColumns(cols);
    }
    C.add(T);
    C.addComponentListener(T);
    C.setVisible(true);
    return T;
  }

  /* : in Prolog + Reflection
     get_text(JinniText,Answer):  collects
     the content of the text area to new constant Answer
  */
  

/*
  in Prolog:
  add_text
  set_text
  get_text
  clear_text
*/
  
  public static Color new_color(double r,double g,double b) {
    if(r>1||r<0) {Interact.warnmes("new_color arg 1 should be in 0..1->"+r);}
    if(g>1||g<0) {Interact.warnmes("new_color arg 2 should be in 0..1->"+g);}
    if(b>1||b<0) {Interact.warnmes("new_color arg 3 should be in 0..1->"+b);}
    int R=(int)(r*255.0);
    int G=(int)(g*255.0);
    int B=(int)(b*255.0);
    Color C=new Color(R,G,B);
    return C;
  }

  
  //  set_fg,set_bg,set_color : in Prolog 
  
 
  public static void set_direction(Container C,String direction) {
    if(C instanceof JinniFrame) 
      ((JinniFrame)C).setDirection(direction);
    else
      ((JinniPanel)C).setDirection(direction);
  }

  public static void destroy(JComponent C) {
    //C.dispose();
    if(C instanceof Container) ((Container)C).removeAll();
    C.removeNotify();
  }

  public static void set_layout(Container C, String layoutName,int x,int y) {
    //C.removeAll();
    LayoutManager L=to_layout(layoutName,x,y);
    C.setLayout(L);
  }

  synchronized public static void show(Container C) {
    C.validate();
    C.setVisible(true);
    // do not do this
    //if(C instanceof JinniFrame) ((JinniFrame)C).pack();
   }
  
  public static void resize(JComponent C,int h,int v) {
    C.setSize(h,v);
  }

  public static void move(JComponent C,int hpos,int vpos) {
    C.setLocation(hpos,vpos);
  }
  
  /**
    detects if applet and gets applet container
  */
  
  public static Applet get_applet() {
    return (Applet)PrologApplet.applet;
  }
 
  public static String get_applet_host() {
    return get_applet().getCodeBase().getHost();
  } 
  
  /*
  public static JinniImagePanel new_image(Container C,String src,
                                          int width,int height) {
   JinniImagePanel P=new JinniImagePanel(src,width,height);
    C.add(P);
    return P;
  }
  */
  
  public static void destroy(Object O) {
		// TODO
		Interact.println("TODO: implement destroy()"+O);
  }
}

class JinniFrame extends JFrame {
  
  JinniFrame(String title,LayoutManager L,int kind) {
     this(title);
     setLayout(L); // hgap=10,vgap=10
     this.kind=kind;
  }

  JinniFrame(String title) {
     super(title);
     this.kind=1;
     this.direction=null;
     setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE); //EXIT_ON_CLOSE
     setSize(Start.defX,Start.defY); // reasonable initial default size  
  }
  
  private int kind;
  private String direction;
                             
  public void setDirection(String direction) {
     this.direction=direction;
  }
  
   public void processEvent(AWTEvent event) {
	 //Interact.println("JFrame event"+event);
     if(this.kind>0 && event.RESERVED_ID_MAX == Event.WINDOW_DESTROY) {
          JComponent Cs[]=(JComponent[]) getComponents();
          for(int i=0;i<Cs.length;i++) {
            JComponent C=Cs[i];
            if(C instanceof JinniButton) {
              JinniButton B=(JinniButton)C;
              B.stop();
            }
          }
          
          dispose();
          //removeNotify();
          removeAll();
        }   
        super.processEvent(event);
   }
  
   public Component add(Component C) {
     if(this.getLayout() instanceof BorderLayout) {
        //Interact.println("adding to: "+direction);
        return super.add(direction,C);
     }
     else {
        //Interact.println("not adding "+C+" to: "+direction+"<="+this);
        return super.add(C);
     }
   }
}

class PrologAction extends AbstractAction {
	private Runnable M;
	private String text;
	
    public PrologAction(String text,Runnable M) {
        super(text);
        this.text=text;
        //putValue(SHORT_DESCRIPTION, desc);
        this.M=M;
    }
    ////
    public void actionPerformed(ActionEvent e) {
        //Prolog.dump("Action="+this.text);
    	Start.invokeLater(M); //$$ M.run();
    }
}

/*
   Examples of Prolog GUI components - add more !
*/

/**
   Button with attached Prolog action.
   Runs action when Button pushed.
*/

////
class JinniButton extends JButton implements Runnable 
{
  JinniButton(String name,Machine M) {
    super(name);
    this.name=name;
    this.M=M;
    Start.setLooks(this);
    PrologAction action=new PrologAction(name,this);
    setAction(action);
  }
  
  private String name;
  private Machine M;
  
  /**
     Passes action to Prolog when Button is pushed
  */
  public void run() {
    ask();
  }
  
  ////
  synchronized private void ask() {
	//Prolog.dump("here");
    int answer=0;
    try {
      if (null!=M) answer=M.ask();
    }
    catch (PrologException e) {
      // ok
    }
    catch (Exception e) {
      // ok - handled in ask
    }
    if(0==answer) Interact.warnmes("the engine attached to a Prolog Button '"+name+"' died");
  }
  
  synchronized public void stop() {
    if(null!=M) {
      M.removeObject(this);
      M.stop();
      M=null;
    }
  }
               
  public void removeNotify() {
    super.removeNotify();
    stop();
  }
}


class JinniPanel extends JPanel {//$$ implements ComponentListener {
  JinniPanel(LayoutManager L) {
    super();
    Start.setLooks(this);
    setLayout(L);
  }

  /* $$ do not do this - defirm jgui.pl assumptions
  public void componentHidden(ComponentEvent e) {
    //Prolog.dump(e.getComponent().getClass().getName()+" --- Hidden");
  }

  public void componentMoved(ComponentEvent e) {
    //Prolog.dump(e.getComponent().getClass().getName()+" --- Moved");
  }

  public void componentResized(ComponentEvent e) {
    //Prolog.dump(e.getComponent().getClass().getName()+" --- Resized ");
    Displayer.smartResize((JComponent)this.getParent(),this,0,0);
  }

  public void componentShown(ComponentEvent e) {
    //Prolog.dump(e.getComponent().getClass().getName()+" --- Shown");
  }
  */
  
  private String direction;
                             
  public void setDirection(String direction) {
    this.direction=direction;
  }
  
  public Component add(Component C) {
     if(this.getLayout() instanceof BorderLayout) {
        //Interact.println("adding to: "+direction);
        return super.add(direction,C);
     }
     else {
        //Interact.println("not adding "+C+" to: "+direction+"<="+this);
        return super.add(C);
     }
  }
  
}
  
/*
class JinniImagePanel extends ImagePanel { 
  private String sourceName;
  private Image image;
  private int width;
  private int height;

  JinniImagePanel(String sourceName,int width,int height) {
    this.sourceName=sourceName;
    this.width=width;
    this.height=height;
    Start.setLooks(this);
    if(null!=PrologApplet.applet) {
      Applet applet=(Applet)PrologApplet.applet;
      URL url=applet.getCodeBase();
      image=applet.getImage(url,sourceName);
    }
    else
      image=Toolkit.getDefaultToolkit().getImage(sourceName);
  }


  // see also (inherited) ImageObserver 

  public void paint(Graphics g) {
      if(width<=0 || height <=0) {
        width=image.getWidth(this);
        height=image.getHeight(this);
      }
      setSize(width,height);
      g.drawImage(image, 0,0, width, height, this); 
  }
}

*/


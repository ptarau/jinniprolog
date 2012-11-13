package prolog3d;

import prolog.logic.Fun; // !?
//import prolog.kernel.Top;
import prolog.core.Cat;
//import java.io.Serializable;

//import prolog.core.*;

import java.applet.Applet;
import java.awt.*;
import java.awt.event.*;
import java.awt.event.WindowAdapter;

import com.sun.j3d.utils.geometry.*;
import com.sun.j3d.utils.universe.*;
import javax.media.j3d.*;
import javax.vecmath.*;
import com.sun.j3d.utils.behaviors.vp.*;


/**
  Jinn3D API 
 */
public class Prolog3D // implements Serializable 
{  
  static private int instCount=0;
  static private Frame sharedFrame=null;
  
  private World world=null;
  private Frame frame=null;
  public TextArea output=null;
  private int inst;
  
  /* returns main Panel */
  public World getWorld() {
    return world;
  }
  
  public Panel addPanel(String whereNSEWC) {
    Panel P=new Panel();
    this.world.add(whereNSEWC,P);
    //this.world.validate();
    return P;
  }
  
  /**
   Creates a new world in a window, not yet visible.
   */
  public Prolog3D(String title,Frame f) {
    super();
    Params.stopAll=false;
    this.inst=instCount++;
    if(Params.reuseTopWindow && null!=sharedFrame && null==f) {
      this.frame=sharedFrame;
    }
    else {
      if(null==f) this.frame=new Frame(title);
      else this.frame=f; // title not used

      frame.setLayout(new BorderLayout());
      frame.validate();
      if(Params.reuseTopWindow) sharedFrame=frame;
    }
    world=new World();
  }
  
  public Prolog3D(Frame frame) {
    this(null,frame);
  }

  public Prolog3D(String title) {
    this(title,null);
  }

  public Prolog3D() {
    this(Params.winTitle);
  }
  /**
   Adds a vertex agent of a given shape to the world
   */
  public Vertex3D addVertex(Node shape,Object data) {
    return new Vertex3D(world,shape,data);
  }
  
  /**
   Adds a vertex agent of a given shape number to the world
   */
  public Vertex3D addVertex(int shapeNo,Color3f c,Object data) {
    return new Vertex3D(world,shapeNo,c,Simple.transparency,data); // $$
  }
  
  public Vertex3D addVertex(int shapeNo,double r,double g,double b,double trans,Object data) {
    return new Vertex3D(world,shapeNo,new Color3f((float)r,(float)g,(float)b),(float)trans,data);
  }

  public Vertex3D addVertex(String label) {
    return new Vertex3D(world,null,label);
  }
  
  /**
   Adds an Edge for 3D Graph Drawing application
  */
  public Edge3D addEdge(Vertex3D From,Vertex3D To,Object label) {
    return new Edge3D(world,From,To,label);
  }
  
  /**
   Adds an Edge represented as a mobile shape to a 3D Graph Drawing application
   */
  public MobileEdge3D addMobileEdge(Vertex3D From,Vertex3D To,Object label) {
    return new MobileEdge3D(world,From,To,label);
  }
  
  /**
   Creates an body agent with a "body" with a HashMap of parts that can be controlled
   individualy
   */
  public Body addBody(String modelFile,Object data) {
    return new Body(world,modelFile,data);
  }
  
  public Satellite addSat(Agent3D from,Node sat,double r,Object data) {
    return new Satellite(world,from,sat,r,data);
  }
  
  public Satellite addSat(Agent3D from,int shapeNo,Color3f c,double r,Object data) {
    return new Satellite(world,from,shapeNo,c,r,data);
  }
  
  /**
    Creates an agent morphing continuously between two shapes
   */
  public Morph3D addMorph(Shape3D[] shapes,Object data) {
    return new Morph3D(world,shapes,data);
  }
  
  /**
   Creates an agent morphing continuously between two models
   */
  public Morph3D addMorph(Fun fnames,Object data) {
    return new Morph3D(world,fnames,data);
  }
  
  public void setView(double x,double y,double z) {
    TransformGroup tg=getWorld().getView(null);
    Transform3D t=new Transform3D();
    tg.getTransform(t);
    
    // modify it here   
    Vector3f pos=new Vector3f((float)x,(float)y,(float)z);
    /*
      trans.setEuler(rot);
      trans.setScale(scale);
      trans.setTranslation(pos);
    */
    t.setEuler(new Vector3d(0,0,0));
    t.setTranslation(pos);
    tg.setTransform(t);
  }
  
  public void setHolder(Agent3D viewHolder) {
    world.setHolder(viewHolder);
  }
  
  public void removeHolder() {
    world.removeHolder();
  }
  
  /**
    Compiles/closes and shows this world in a window.
  */
  public void showWorld(int w,int h) {
    world.run();
    frame.add("Center",world);
    frame.setSize(w,h);
    frame.validate();
    //frame.pack(); do not do this!
    frame.setVisible(true);
    frame.addWindowListener(new WindowAdapter() {
      public void windowClosing(WindowEvent e) {
        stopWorld();
        exit();
      }
    } );
    Params.sleep(1000L);
  }
  
  /**
  Tries to stop all agents and close the window associated with a given world
  */
  
  public void stopWorld() {
    if(0==inst) Params.stopAll=true;
    world.destroy();
    frame.removeAll();
    if(!Params.reuseTopWindow) frame.dispose();
    pp("stopped world("+inst+"):"+this);
  }
  
  /**
     Forces exiting this application.
   */
  public void exit() {
    if((!Params.isApplet()) && 0==inst) System.exit(0);
  }
  
  /**
     Opens a snaphot window ready to be printed or saved
     to a PNG file.
   */
  public void printWorld(int w,int h) {
    world.print(w,h);
  }
  
   /**
     Adds controls to a world.
   */
  public void addOutput(TextArea output) {
    this.output=output;
  }
  
  /**
     Adds controls to a world.
   */
  public void addControls() {
    addControls(null);
  }
  
  /**
   Adds controls to a world created as a result of running a layout engine.
  */
   
  public void addControls(LayoutEngine LG) {
  
    Controls cs;
    if(null==output) {
      // small self contained controls
      cs=new SmallControls(this,LG);
      world.add("South",cs);
    }
    else {
      // controls provided by a Prolog IDE
      cs=new Controls(this,LG); 
    }
    world.controls=cs;
  }
  
  /** 
     Runs a layout engine on an attributed graph/category
     in a world of a given 3D radius
   */
  public void runLayout(int radius) {
    Cat C=world.getAgents();
    //Prolog3D.pp(Cat.showInfo(C));
    //Prolog3D.pp(C.toString());
    LayoutEngine L=new SmartLayout(this,C,radius,false);
    L.run();
  }
  
  /** 
     Draws a ranked graph with default parameters
   */
  static public void drawGraph(Cat RG) {
    drawGraph(RG,20,300,400,400);
  }  
   
  /**
     Runs a layout algorithm  on a ranked graph and displays it for a
     given time (in seconds). 
   */  
  static public void drawGraph(Cat RG,int time,int radius,int w,int h) {
    Params.bgfile="";
    Prolog3D M=new Prolog3D();
    RG.runGraphRanker();
    LayoutEngine L=new SmartLayout(M,RG,radius,true);
    M.addControls(L);
    M.showWorld(w,h);
    L.run();
    if(time>0) {
      Params.sleep(1000L*time);
      M.stopWorld();
    }
  }
  
   /**
     Runs a layout algorithm on a ranked graph in an existing universe,
     without animating the layout.
   */  
  static public void catModel(Prolog3D M,int radius,Cat RG,String bgfile) {
    Params.bgfile=bgfile;
    RG.runGraphRanker();
    RG.rankSort();
    RG.markComponents();
    LayoutEngine L=new SmartLayout(M,RG,radius,true);
    L.run();
  }
  
  static public void catModel(Prolog3D M,int radius,Cat RG) {
    catModel(M,radius,RG,"");
  }

  /**
   Runs a layout algorithm on a ranked graph in an existing universe,
   without animating the layout.
   */  
  static public void drawTree(Prolog3D M,int radius,Cat RG,String bgfile) {
    Params.bgfile=bgfile;
    RG.runDualGraphRanker();RG.rankSort();RG.markComponents();
    LayoutEngine L=new TreeLayout(M,RG,radius,true);
    L.run();
  }
  
  static public void drawTree(Prolog3D M,int radius,Cat RG) {
    drawTree(M,radius,RG,"");
  }
  
  /**
     Runs a layout algorithm  on a ranked graph and displays it for a
     given time (in seconds), after representing it as a frowzed geometry object. 
   */
  static public void drawFrozenGraph(Cat RG,int time,int r,int w,int h) {
    Params.bgfile="";
    Params.bgColor=new Color3f(0,0,0);
    Prolog3D M=new Prolog3D();
    RG.runGraphRanker();
    LayoutEngine L=new SmartLayout(M,RG,r,true);
    M.addControls(L);
    M.showWorld(w,h);
    L.run();
    toFrozen(L,w,h);
    if(time>0) {
      Params.sleep(1000L*time);
      M.stopWorld();
    }
  } 
  
  /**
      Converts a graph layout to an efficient point/line geometry representation.
   */
  static public void toFrozen(LayoutEngine L,int w,int h) { 
    FrozenGraph S=new FrozenGraph(L);
    Prolog3D M=new Prolog3D();
    Vertex3D A=M.addVertex(S,null);
    M.addControls(null);
    M.showWorld(w,h);
  }
  
  /**
     Prints a message (usually for tracing) to the standard error stream.
   */
  static public void pp(Object O) {
    System.err.println(O.toString());
  }
  
  /**
     Main method that can be used to start a self contained application
   */

  public static void main0(String[] args) {
    Prolog3D M=new Prolog3D();
    Vertex3D V=M.addVertex("one");
    V.setAuto(1);
    M.showWorld(400,400);
    Params.sleep(10000);
    M.stopWorld();
    M.exit();
  }

  
  public static void main(String[] argv) {
    //Top.ZIPSTORE="prolog3d.jar";
    (new prolog.kernel.Shell(argv,null,null,true)).run();
  }
  
}

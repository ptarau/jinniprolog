package prolog3d;

import javax.media.j3d.*;
import javax.vecmath.*;
import java.util.*;
import java.awt.event.*;
import java.awt.*;
import com.sun.j3d.utils.picking.*;

/**
 * Agents that form vertices of a Graph - this is usefult for applying automated layout
 * algorithms to them - for instance and to express connections between them as Edge3D objects.
 */
public class Vertex3D extends Agent3D {
  
  public Vertex3D(World world,Node shape,Object data) {
    super(world,data);
    if(null==shape) shape=new Sphere3D(Simple.defCol);
    shape.setPickable(true);
    this.shape=shape;
    addChild(shape);
    R=G=B=0.5;
  }
  
  /**
   * Creates a vertex of a predefined shape in a given world
   */
  public Vertex3D(World world,int shapeNo,Color3f c,float t,Object data) {
    this(world,(new Shape(c,t)).toNode(shapeNo,data),data);
  }
 
  public Vertex3D(World world,int shapeNo,double r,double g,double b,double t,Object data) {
    this(world,shapeNo,new Color3f((float)r,(float)g,(float)b),(float)t,data);
  }

  final public Node shape;
  double R,G,B;

  public void setColor(double red,double green,double blue) {
    if(shape instanceof Simple) {
      R=red;G=green;B=blue;
      Simple S=(Simple)shape;
      S.transformColor(red,green,blue);
    }
  }

  private static final double newColor(double color,double inc) {
    double newcolor=color+inc;
    if(newcolor>1.0) newcolor=1.0;
    else if(newcolor<0.0) newcolor=0.0;
    return newcolor;
  }

  public void incRed(double inc) {
    double newcolor=newColor(R,inc);
    if(newcolor!=R) {R=newcolor;setColor(R,G,B);}
  }

  public void incGreen(double inc) {
    double newcolor=newColor(G,inc);
    if(newcolor!=G) {G=newcolor;setColor(R,G,B);}
  }

  public void incBlue(double inc) {
    double newcolor=newColor(B,inc);
    if(newcolor!=B) {B=newcolor;setColor(R,G,B);}
  }

  private static final double delta=0.01;
  public void autoColor() {
    int choice=Params.ri(100);
    if(0!=choice) return;
    choice = choice % 6;
    switch(choice) {
      case 0: incRed(delta); break;
      case 1: incRed(-delta); break;
      case 2: incGreen(delta); break;
      case 3: incGreen(-delta); break;
      case 4: incBlue(delta); break;
      case 5: incBlue(-delta); break;
      default: /* do nothing */
    }
  }

  public void setWakeUp(int tick) {
    if(!(shape instanceof Simple)) setWakeUp1(tick);
    else setWakeUp2(tick);
  }
  
  public void checkEventOnShape(Node N,MouseEvent e) {
    if(N.equals(this.shape)) {
      onClick();
      int b=e.getButton();
      if(MouseEvent.BUTTON1==b) onLeftClick();
      else onRightClick();
      //else Prolog3D.pp("unexpected click at x="+e.getX()+",y="+e.getY()+N+"!="+e);
    }
    else {
      //Prolog3D.pp("x="+e.getX()+",y="+e.getY()+N+"!="+this.shape);
    }
  }
  
  public void onClick() {
  }
  
  public void onLeftClick() {
    print(this);
  }
  
  public void onRightClick() {
    String s=
      getClass().getName()+
      "\nX="+getX()+"\nY="+getY()+"\nZ="+getZ()+")\n(R,G,B)=("+R+","+G+","+B+")";
    print(s);
  }
}

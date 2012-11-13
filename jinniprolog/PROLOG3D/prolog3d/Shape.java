package prolog3d;

import javax.media.j3d.*;
import com.sun.j3d.utils.geometry.*;
import javax.vecmath.*;
import java.awt.Font;
import java.util.*;
import com.sun.j3d.utils.picking.*;

public class Shape {
  
  private Appearance app;
  private Color3f col;
  private float trans;

  Shape() {
    this(null);
  }
  
  Shape(Color3f col,float trans) {
    if(null==col) col=Simple.defCol;
    this.col=col;
    this.trans=trans;
    this.app=Simple.makeApp(col,trans);
  }
  
  Shape(Color3f col) {
    this(col,Simple.transparency);
  }

  public static final int SPHERE=0;
  public static final int POINT=1;
  public static final int LINE=2;
  public static final int TETRA=3;
  public static final int CUBE=4;
  public static final int CCUBE=5;
  public static final int PYRAM=6;
  public static final int GRAPH=7;

  public static final int maxSimple=8;

  public static final int CONE=maxSimple+0;
  public static final int CYLINDER=maxSimple+1;
  public static final int TEXT2D=maxSimple+2;
  public static final int TEXT3D=maxSimple+3;
  public static final int TEXT2Dno=maxSimple+4;
  public static final int TEXT3Dno=maxSimple+5;
  public static final int MODEL=maxSimple+6;
  public static final int XMODEL=maxSimple+7;
  

  private Simple toSim(int shape) {
    Simple ob;
    //shape=0;
    switch(shape) {
      case SPHERE:
        ob=new Sphere3D(col,trans); 
        break;
      case POINT:
        ob=new Point(col,trans);
        break;      
      case LINE: 
        ob=new Line(col,trans);      //
        break;  
      case TETRA:
        ob=new Tetra(col,trans);
        break;
      case CUBE:
        ob=new Cube(col,trans);
        break;
      case CCUBE:
        ob=new CCube();  
        break;
      case PYRAM:
        ob=new Pyram(col,trans);
        ob.transformScale(2.0);
        break;
      case GRAPH: 
        //Prolog3D.pp("!!!!here");   
        ob=FrozenGraph.random();
        break;
      default:
        ob=null;
        Prolog3D.pp("unknown shape: "+shape+",color="+col);      
    }
    return ob;
  }
  
  Node toNode(int shape,Object data) {
    Node ob=null;  
    if(shape<maxSimple) return toSim(shape);
    if(null==data) data="?";
    String info=data.toString();
    float r=0.1f;
    float h=0.4f;
    switch(shape) {
      case CONE:
        ob=new Cone(r,3*h,app); //8
        break;
      case CYLINDER: //9
        ob=new Cylinder(r,h,app);
        break;
      case TEXT2D: //10
        ob=text2D(info,true);
        break;  
      case TEXT3D: //11
        ob=text3D(info,true);
        break;
      case TEXT2Dno: //12
        ob=text2D(info,false);
        break;  
      case TEXT3Dno: //13
        ob=text3D(info,false);
        break;          
      case MODEL: //14 
        // handles formats like *.j3f *.xml *.wrl and their *.<form>.gz variants
        // as well as *.obj (which should not be gzipped as Sun's loader assumes a plain file)
        ob=Convert.file2bgroup(info); //12
        break;
      case XMODEL: //15
        // handles formats like *.xml exported from Blender and *.xml.gz variants
        ob=Convert.xml2shape(info); //13
        break;  
      /*case maxSimple+6:
        ob=Prolog3D.addMorph((Fun)data,"morph"); //14
        break;   
      */
      default:
        Prolog3D.pp("unknown node: "+shape+",data="+data);      
    }
    return ob;
  }
  
  static String trimText(String text) {
    if(Params.maxText>0 && Params.maxText<text.length()) text=text.substring(0,Params.maxText)+"...";
    return text;
  }

  static TransformGroup text2D(String text,boolean showCenter) { 
    text=trimText(text);
    Transform3D s=new Transform3D();
    TransformGroup sbox=new TransformGroup(s);
    Billboard B=new Billboard(sbox);
    //Color3f col=new Color3f(1.0f,1.0f,0.0f);       
    Text2D txt=new Text2D(text,Params.textColor,"Serif",100,Font.PLAIN);
    txt.setRectangleScaleFactor(0.005f);
    if(showCenter) {
      Sphere3D x=new Sphere3D();
      x.transformScale(0.1);
      sbox.addChild(x);
      sbox.setTransform(s);
    }
    sbox.addChild(txt);
    return sbox;
  }  
  
  static TransformGroup text3D(String text,boolean showCenter) {
    text=trimText(text);
    Transform3D s=new Transform3D(); 
    TransformGroup sbox=new TransformGroup(s);
    //s.setScale(2.0);
    if(showCenter) {
      Sphere3D x=new Sphere3D();
      x.transformScale(0.1);
      sbox.addChild(x);
      sbox.setTransform(s);
    }
    float sl = text.length();
    Font3D f3d = new Font3D(new Font("Serif", Font.PLAIN, 2),
      new FontExtrusion());
    Text3D txt = new Text3D(f3d,text, 
      new Point3f(-sl/2.0f, -1f, -1f));
    //txt.setCapability(Shape3D.ALLOW_GEOMETRY_READ);
    
    OrientedShape3D sh = new OrientedShape3D();
    //sh.setCapability(Node.ENABLE_PICK_REPORTING); 
    sh.setAlignmentAxis( 0.0f, 1.0f, 0.0f);
    sh.setGeometry(txt);
    sh.setAppearance(Params.textApp);
    
    Transform3D t=new Transform3D();
    t.setScale(0.08);
    t.setTranslation(new Vector3f(0.0f,0.4f,0.0f));
    TransformGroup tbox=new TransformGroup(t);
    //tbox.setCapability(Node.ENABLE_PICK_REPORTING);
    tbox.addChild(sh);
   
    sbox.addChild(tbox); 
     
    return sbox;  
  }   
}
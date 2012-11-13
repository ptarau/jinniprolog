package prolog3d;

import javax.media.j3d.*;
import javax.vecmath.*;
import com.sun.j3d.utils.picking.*;

public class Simple extends TransformGroup {
  public static double defScale=0.2;

  public static float transparency=0.0f;

  public static Color3f defCol=new Color3f(1.0f,1.0f,0.0f);

  public static Appearance defApp=makeApp(defCol,transparency);
  
  public static final Vector3f zero=new Vector3f(0,0,0);
  public static final Vector3f oneX=new Vector3f(1,0,0);
  public static final Vector3f oneY=new Vector3f(0,1,0);
  public static final Vector3f oneZ=new Vector3f(0,0,1);
    
  Shape3D shape3d;
  Transform3D localTransform;
  
  public Simple(Shape3D shape3d,Appearance app3d) {
    super();
    this.shape3d=shape3d;
    this.localTransform=new Transform3D();
    shape3d.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
    shape3d.setAppearance(app3d);
    PickTool.setCapabilities(shape3d,PickTool.INTERSECT_FULL);
    setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
    setCapability(Node.ENABLE_PICK_REPORTING);
    addChild(shape3d);
  }

  public Simple(Geometry geom,Appearance app3d) {
    this(new Shape3D(geom),app3d);
  }
  
  public Simple(Shape3D shape3d) {
    this(shape3d,defApp);
  }

  public void transformPosition(double x,double y,double z) {
    localTransform.setTranslation(new Vector3d(x,y,z));
    setTransform(localTransform);
  }
  
  public void transformRotation(double x,double y,double z) {
    localTransform.setEuler(new Vector3d(x,y,z));
    setTransform(localTransform);
  }
  
  public void transformScale(double x,double y,double z) {
    //Prolog3D.pp("scaling to "+x+","+y+","+z);
    //Transform3D T=new Transform3D();
    localTransform.setScale(new Vector3d(x,y,z));
    setTransform(localTransform);
  }
  
  public void transformScale(double s) {
    transformScale(s,s,s);
  }
  
  public void transformColor(double r,double g,double b) {
     //Material mat=app3d.getMaterial();
     //Material mat=new Material();
     //mat.setDiffuseColor((float)r,(float)g,(float)b);
     //app3d.setMaterial(mat);
     Appearance app3d=makeApp(new Color3f((float)r,(float)g,(float)b));
     shape3d.setAppearance(app3d);
     //Prolog3D.pp("here="+this+"="+r+","+g+","+b);
  }

  static public Appearance makeApp() {
    return makeApp(defCol);
  }
  
  public void draw(Tuple3f dif,float r) {}
  
  static public Appearance makeApp(Color3f col) {
    return makeApp(col,transparency);
  }
  
  static public Appearance makeApp(Color3f col,float transparency) {
    if(null==col) {
      col=defCol;
      //Prolog3D.pp("!!! using defColor");
    }
    Appearance a=new Appearance();
    
    Material mat=new Material();
    mat.setLightingEnable(true);
    mat.setDiffuseColor(col);
    a.setMaterial(mat);
    
    if(transparency>0.01f) {
      TransparencyAttributes ta = new TransparencyAttributes();
      ta.setTransparencyMode(ta.BLENDED);
      ta.setTransparency(transparency);
      a.setTransparencyAttributes(ta);
    }
    
    // Set up point attributes
    PointAttributes pta = new PointAttributes();
    pta.setPointSize(3.0f);
    a.setPointAttributes(pta);
    
    LineAttributes la = new LineAttributes();
    la.setLineWidth(1.0f);
    a.setLineAttributes(la);
    
    return a;
  }
  
}
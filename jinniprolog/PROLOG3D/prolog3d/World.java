package prolog3d;
import prolog.core.Cat;

import java.applet.Applet;
import java.awt.*;
import java.awt.event.*;
import java.awt.event.WindowAdapter;
import com.sun.j3d.utils.geometry.*;
import com.sun.j3d.utils.image.TextureLoader;
import com.sun.j3d.utils.universe.*;
import javax.media.j3d.*;
import javax.vecmath.*;
import com.sun.j3d.utils.picking.*;
import java.io.File;

public class World extends Panel  {
 
  //public static Color3f bgColor = new Color3f(0.1f, 0.1f, 0.5f);
  
  public static BoundingSphere bounds=new BoundingSphere(new Point3d(0.0, 0.0, 0.0), 100.0);
  
  private SimpleUniverse u = null;
  private BranchGroup scene;
  
  private Canvas3D canvas;
  private PickCanvas pCanvas;
  Controls controls;
  private final Cat agents;
  private final Cat objects;
  private Agent3D viewHolder;
  
  public World() {
    super();
    viewHolder=null;
    agents=new Cat();
    objects=new Cat();
    makeUniverse();
    this.scene = createSceneGraph();
  }
  
  public BranchGroup getScene() {
    return scene;
  }
  
  public Canvas3D getCanvas() {
    return canvas;
  }
  
  public Cat getAgents() {
    return agents;
  }
  
  public Cat getObjects() {
    return objects;
  }
    
  public PickCanvas getPickCanvas() {
    return pCanvas;
  }
  
  public void run() {
    scene.compile();
    u.addBranchGraph(scene);
  }
  
  public void destroy() {
    //Params.stopAll=true;
    Prolog3D.pp("start destroying world:"+this);
    u.cleanup();
    u.removeAllLocales();
    canvas.stopRenderer();
    Prolog3D.pp("finished destroying world:"+this);
  }
  
  public void print(Object O) {
    String s=O+"\n";
    if(null!=controls) controls.print(s);
    else Prolog3D.pp(s);
  }
  
  public BranchGroup createSceneGraph() {
    String fname=Params.bgfile;
    BranchGroup objRoot = new BranchGroup();
    //objRoot.setCapability(BranchGroup.ALLOW_DETACH);
    
    fname=Convert.fixFileName(fname);  
    if(null==fname) createPlainBackground(objRoot);
    else createImageBackground(fname,objRoot);
       
    createLights(objRoot);
    //ColorCube C0=new ColorCube(0.1f);objRoot.addChild(C0);
    this.pCanvas=new PickCanvas(this.canvas,objRoot);
    pCanvas.setMode(PickTool.GEOMETRY_INTERSECT_INFO); 
    //pCanvas.setMode(PickTool.BOUNDS); 
    pCanvas.setTolerance(1.0f);
    return objRoot;
  }
  
  public void createLights(BranchGroup objRoot) {
    AmbientLight aLgt = new AmbientLight(Params.bgAmbientColor);
    aLgt.setInfluencingBounds(bounds);
    DirectionalLight lgt = new DirectionalLight(Params.bgLightColor, Params.bgLightDir);
    lgt.setInfluencingBounds(World.bounds);
    objRoot.addChild(aLgt);
    objRoot.addChild(lgt);
  }
  
  public void createPlainBackground(BranchGroup objRoot) {
    Background bg = new Background(Params.bgColor);
    bg.setApplicationBounds(World.bounds);
    objRoot.addChild(bg);
  }
  
  public void createImageBackground(String fname,BranchGroup objRoot) {
    TransformGroup objTrans = new TransformGroup();
    Background bg = new Background();
    bg.setApplicationBounds(World.bounds);
    BranchGroup backGeoBranch = new BranchGroup();
    
    Sphere S = new Sphere(1.0f, Sphere.GENERATE_NORMALS |
      Sphere.GENERATE_NORMALS_INWARD |
      Sphere.GENERATE_TEXTURE_COORDS, 50);
        
    Appearance backgroundApp = S.getAppearance();
    backGeoBranch.addChild(S);
    TextureLoader tex=null;
    if(!fname.startsWith("http://")) 
      tex = new TextureLoader(fname,this);
    else {
      try {
        tex=new TextureLoader(new java.net.URL(fname),this);
      }
      catch(Exception e) {
      }
    }
    if (tex != null) backgroundApp.setTexture(tex.getTexture());
    bg.setGeometry(backGeoBranch);
    objTrans.addChild(bg);
    objRoot.addChild(objTrans);
  }
    
  public void print(int w,int h) {
    BranchGroup b=getScene();
    //b.detach();
    destroy(); 
    if(Params.reuseTopWindow) ((Frame)getParent()).dispose();
    PrintCanvas3D.print(b,w,h);
  }

  public void makeUniverse() {
    setLayout(new BorderLayout());
    GraphicsConfiguration config =
      SimpleUniverse.getPreferredConfiguration();
    Canvas3D c = new Canvas3D(config);
    this.canvas=c;
    add("Center", c);
    u = new SimpleUniverse(c);
    u.getViewingPlatform().setNominalViewingTransform(); 
    if(Params.interactive==1)
      u.getViewingPlatform().setViewPlatformBehavior(new LocalOrbit(c));
  }
  
 
  /**
     Sets an agent as the "holder" of the view - i.e. the view will
     be animated following the agent's movements, rotations etc.
  */
  synchronized public void setHolder(Agent3D viewHolder) {
    this.viewHolder=viewHolder;
  }
  
  public void removeHolder() {
    setHolder(null);
  }
  
  /**
     Returns a handle to the TransformGroup that allows view animation.
     Use getTransform and setTransform on this handle to manipulate the view.
  */
  
  public TransformGroup getView(Agent3D viewHolder) {
    if(viewHolder!=this.viewHolder) return null; 
    return u.getViewingPlatform().getViewPlatformTransform();
  }
}

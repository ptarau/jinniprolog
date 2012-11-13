package prolog3d;

import javax.media.j3d.*;
import javax.vecmath.*;
import com.sun.j3d.utils.geometry.*;
import prolog.logic.Fun; //?

/**
  Imports Morphs created with Blender and exported to XML
  representations of Shape3D objects. Objects should have
  "similar structure" as specified by class Morph 
 */
public class Morph3D extends Agent3D { 
  
  public Morph3D(World world,Shape3D[] shapes,Object data) {
    super(world,data);
    this.data=data;
    this.t=0;
    setWakeUp(100);
    
    this.morph=shapes2morph(shapes);
      
    addChild(morph);
    scaleTo(3.0);
  }
  
  public Morph3D(World world,Fun fnames,Object data) {
    this(world,Convert.xml2shapes(fnames),data);
  }
    
  public static Morph shapes2morph(Shape3D[] shapes) {
    int l=shapes.length;
    GeometryArray[] gs=new GeometryArray[l];
    for(int i=0;i<l;i++) {
      Shape3D shape=shapes[i];
      gs[i]=(GeometryArray)shape.getGeometry();
    }
    Morph morph = new Morph(gs, Simple.defApp);
    morph.setCapability(Morph.ALLOW_WEIGHTS_READ);
    morph.setCapability(Morph.ALLOW_WEIGHTS_WRITE);
    double[] weights=new double[l];
    weights[0]=1.0;
    morph.setWeights(weights);
    return morph;
  }
  
  Object data; 
  Morph morph;
  
  int k=0;
  float t=0;
  float delta=0.05f;
  
  float dt() {
    if(t<1) t+=delta;
    else t=0;
    return t;
  }
   
  public void transform() {
    double weights[]=morph.getWeights();
     
    int l=weights.length;
    for(int i=0;i<l;i++) {
      weights[i]=0.0f;
    }
    float a=dt();
    if(a==0) k=(k+1) % l;
  
    int m=k;
    int n=(k+1) % l;
      
    weights[m]=1-a;
    weights[n]=a;
    
    morph.setWeights(weights);
    //return false;
  }
   
  public void onLeftClick() {
    print(data);
  }
  
  public String toString() {
    return data.toString();
  }
}


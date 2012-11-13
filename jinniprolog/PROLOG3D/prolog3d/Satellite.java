package prolog3d;

import javax.media.j3d.*;
import javax.vecmath.*;
import com.sun.j3d.utils.geometry.*;

/**
  Satellite agents rotate around their masters. Still, as all agents,
  they can implement their own touch of a specialized behavior.
  The construct is reentrant - satellites can have satellites themselves.
 */
public class Satellite extends Agent3D {
  
  public void addToCat() {
  }
  
  public Satellite(World world,Agent3D From,Node sat,double r,Object data) {  
    super(world,data,false);
    this.From=From;
    getWorld().getAgents().setProp(From,this,"satellite");
    alpha=0;
    Transform3D wtrans=new Transform3D();
    wtrans.setTranslation(new Vector3d(-r,0,0));
    this.wrapper=new TransformGroup(wtrans);
    wrapper.addChild(sat);
    super.addChild(wrapper);
    From.addChild(getGroup());
  }
  
  public Satellite(World world,Agent3D From,int shapeNo,Color3f c,double r,Object data) {
    this(world,From,(new Shape(c)).toNode(shapeNo,data),r,data);
  }
 
  TransformGroup wrapper;

  public void addChild(Node child) {
    wrapper.addChild(child);
  }

  Agent3D From;
  double alpha;
  
  public void transform() {
    float q=0.05f;
    alpha+= q+Params.rf(q);
    if(alpha>2*Math.PI) alpha=0;
    setRotY(alpha);
  }
   
  public void onLeftClick() {
    print(data);
  }
  
  public void onRightClick() {
    print(From+"==>"+this);
  }
  
  public String toString() {
    return data.toString();
  }
}


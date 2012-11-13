package prolog3d;

import javax.media.j3d.*;
import javax.vecmath.*;
import com.sun.j3d.utils.geometry.*;

public class MobileEdge3D extends Vertex3D {
  static Color3f edgeCol=new Color3f(1f,1f,0f);
  
  public void addToCat() {
  }
  
  public MobileEdge3D(World world,Vertex3D From,Vertex3D To,Object data) {
    super(world,new Sphere3D(new Color3f(1,1,1),0.0f),data);
    this.From=From;
    this.To=To;
    getWorld().getAgents().setMorphism(From,To,this,data);
    this.t=0;
    scaleTo(0.2);
  }
  
  Vertex3D From;
  Vertex3D To;
  
  float t;
  float delta=0.05f;
  
  float dt() {
    if(t<1) t+=delta;
    else t=0;
    return t;
  }
  
  public void transform() {
    Vector3f from=From.getPos();
    Vector3f to=To.getPos();
    getPos().interpolate(from,to,dt());
  }
   
  public void onLeftClick() {
    print(data);
  }
  
  public void onRightClick() {
    print(From+"==>"+To);
  }
  
  public String toString() {
    return data.toString();
  }
}


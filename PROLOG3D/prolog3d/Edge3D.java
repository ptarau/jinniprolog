package prolog3d;

import javax.media.j3d.*;
import javax.vecmath.*;
import com.sun.j3d.utils.geometry.*;

public class Edge3D extends Vertex3D {
  static Color3f edgeCol=new Color3f(0.5f,0.5f,0.5f);
  
  public void addToCat() {
  }
  
  public Edge3D(World world,Vertex3D From,Vertex3D To,Object data,Simple shape) {
    super(world,shape,data);
    this.From=From;
    this.To=To;
    getWorld().getAgents().setMorphism(From,To,this,"edge");
  }
  
  public Edge3D(World world,Vertex3D From,Vertex3D To,Object data) {
    this(world,From,To,data,new Pyram(edgeCol,0.5f));
  }
  
  public Edge3D(World world,Vertex3D From,Vertex3D To,Object data,double r,double g,double b) {
    this(world,From,To,data,new Line(new Color3f((float)r,(float)g,(float)b)));
  }
  
  public Edge3D(World world,Vertex3D From,Vertex3D To,Object data,double r,double g,double b,double t) { 
    this(world,From,To,data,new Pyram(new Color3f((float)r,(float)g,(float)b),(float)t));
  }
    
  Vertex3D From;
  Vertex3D To;
 
  public void transform() {
    Vector3f from=From.getPos();
    Vector3f to=To.getPos();
    Vector3f dif=new Vector3f(); 
    dif.sub(to,from);
    ((Simple)shape).draw(dif,0.06f*(float)From.getScaleX());
    moveTo(from.x,from.y,from.z);
  }
  
  public void onClick() {
  }
  
  public void onLeftClick() {
  }
  
  public void onRightClick() {
  }
  
  public String toString() {
    return data.toString();
  }
}


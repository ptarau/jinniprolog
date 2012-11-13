package prolog3d;

import javax.media.j3d.*;
import javax.vecmath.*;


public class PyramShape extends Shape3D {

  public PyramShape() {
    this(null);
  }
  
  public PyramShape(Color3f color) {
    //Prolog3D.pp(this+":"+color);
    this.geom=new PyramGeom(color);
    this.setGeometry(geom);
    this.setAppearance(new Appearance());
  }
  
  PyramGeom geom;
}

class PyramGeom extends QuadArray {

  PyramGeom(Color3f color) {
    super(24, QuadArray.COORDINATES|QuadArray.COLOR_3|GeometryArray.ALLOW_COORDINATE_WRITE);
    setCapability(GeometryArray.ALLOW_COORDINATE_WRITE);
    makeFaces(0.06f,0.0f,0.2f,0.0f);
    if(color==null) color=Simple.defCol; 
    makeColors(color);
  }
  
  private void makeColors(Color3f c) {
  
    Color3f[] colors=new Color3f[24];
    int l=colors.length;
    for(int i=0;i<l;i++) {
      colors[i]=c;
    }
    setColors(0,colors);
  }
  
  void makeFaces(float r,float x,float h,float z) {
    // bottom face
    Point3f A=new Point3f(-r,0,r);
    Point3f B=new Point3f(-r,0,-r);
    Point3f C=new Point3f(r,0,-r);
    Point3f D=new Point3f(r,0,r);
    Point3f V=new Point3f(x,h,z);  
    Point3f[] verts=
      {
        D,V,V,A, //front
        B,V,V,C, //back
        C,V,V,D, //right
        A,V,V,B, //left
        V,V,V,V, //top
        A,B,C,D  //bottom
      };    
    setCoordinates(0, verts);
  }
  
  public void draw(Tuple3f dif,float r) {
    makeFaces(r,dif.x,dif.y,dif.z);
  }
 
}

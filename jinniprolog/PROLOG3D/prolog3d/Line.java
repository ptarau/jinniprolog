package prolog3d;

import javax.media.j3d.*;
import javax.vecmath.*;

public class Line extends Simple {
  Line() {
    super(new LineShape(),Simple.defApp);
  }
  
  Line(Color3f c) {
    super(new LineShape(c));
  }
  
  Line(Color3f c,float t) {
    super(new LineShape(c),makeApp(c,t));
  }
  
  public void draw(Tuple3f dif,float ignore) {
    ((LineShape)shape3d).geom.draw(dif);
  }
}

class LineShape extends Shape3D {

  public LineShape() {
    this(null);
  }
  
  public LineShape(Color3f color) {
    this.geom=new LineGeom(color);
    this.setGeometry(geom);
    this.setAppearance(new Appearance());
  }
  
  LineGeom geom; 
}

class LineGeom extends LineArray {
  LineGeom(Color3f color) {
    super(4, GeometryArray.COORDINATES | GeometryArray.COLOR_3 | GeometryArray.ALLOW_COORDINATE_WRITE);  
    setCapability(GeometryArray.ALLOW_COORDINATE_WRITE);
     
    this.verts = new Point3f[2];
        
    verts[0] = new Point3f(0f,0.0f,0f);
    verts[1] = new Point3f(0f,1.0f,0f);
    
    setCoordinates(0, verts);  
    
    if(null==color) color=Simple.defCol; 
    {
      Color3f colors[] = new Color3f[2];
      colors[0] = color;
      colors[1] = color;
      setColors(0, colors); 
    } 
  }
  
  final Point3f verts[];
   
  public void draw(Tuple3f dif) {
    verts[0] = new Point3f(Simple.zero);
    verts[1] = new Point3f(dif);
    setCoordinates(0, verts);
  }
}


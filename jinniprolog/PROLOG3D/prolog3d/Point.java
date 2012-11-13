package prolog3d;

import javax.media.j3d.*;
import javax.vecmath.*;

public class Point extends Simple {
  Point() {
    super(new PointShape(defCol),Simple.defApp);
  }
  
  Point(Color3f c,float t) {
    super(new PointShape(c),makeApp(c,t));
  }
  
  Point(Color3f c) {
    super(new PointShape(c));
  }
}

class PointShape extends Shape3D {
  public PointShape(Color3f c) {
    this.setGeometry(new PointGeom(c));
    this.setAppearance(new Appearance());
  }
}

class PointGeom extends PointArray {

  PointGeom(Color3f c) {
    super(1, GeometryArray.COORDINATES | GeometryArray.COLOR_3);  
    
    Point3f verts[] = new Point3f[1];
    Color3f colors[] = new Color3f[1];
    
    verts[0] = new Point3f(0.0f,0.0f,0.0f);
    if(null==c) c=Simple.defCol;
    colors[0] = c;
   
    setCoordinates(0, verts);
    setColors(0, colors);
  }
}


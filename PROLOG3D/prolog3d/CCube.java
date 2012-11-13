package prolog3d;

import javax.media.j3d.*;
import javax.vecmath.*;
import com.sun.j3d.utils.geometry.*;

/**
   A Color Cube
 */
public class CCube extends Simple {
  public CCube() {
    super(new ColorCube(defScale));
  }
}


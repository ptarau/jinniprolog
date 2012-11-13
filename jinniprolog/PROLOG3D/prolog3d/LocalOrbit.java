package prolog3d;

import java.awt.*;
import java.awt.event.*;
import com.sun.j3d.utils.behaviors.vp.*;
import javax.media.j3d.*;
import javax.vecmath.*;

public class LocalOrbit extends OrbitBehavior 
{
  /**
     Extensible Orbit handling view changes. Note that ALT-LeftMouse
     should be used for zooming !!!
   */
  public LocalOrbit(Canvas3D c) {
    super(c,REVERSE_ALL|PROPORTIONAL_ZOOM);//|KEY_LISTENER);  
    setSchedulingBounds(World.bounds);
  }
  
  //public void keyTyped(KeyEvent e) {Prolog3D.pp("event:"+e);}
  
  // note: also has Mouse events - it can be made to handle all mouse eents
  // for agents - possibly more efficiently
 
}
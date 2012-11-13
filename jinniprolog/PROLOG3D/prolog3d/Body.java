package prolog3d;

import javax.media.j3d.*;
import javax.vecmath.*;
import java.util.*;

/**
 * A Body is a Vertex3D Agent providing individual control on a set of joints.
 * By setting the focus one one of them at atime, the transforms sent to the body
 * will apply to the given joint. That overloads elegantly the usual API to cover
 * Body agents with joints. Dor Humanoids (of course!) only rotations must used - 
 * otherwise you will end up braking them apart :-)
 */
public class Body extends Vertex3D {
  
  public Body(World world,String[] FileOrURLs,Object data) {
    super(world,Convert.file2bgroup(FileOrURLs),data);
    
    HashMap H=null;
    try {
      H=(HashMap)(((BranchGroup)shape).getUserData());
    }
    catch(Exception e) {
      Prolog3D.pp("warning: not a humanoid:"+e);
    }
    
    if(null==H) H=new HashMap();
    else Convert.fixMap(H);
    
    this.transforms=H;
    this.focus=null; 
  }
  
  public Body(World world,String FileOrURL,Object data) {
    //this(world,toStrings(FileOrURL),data);
    this(world,new String[]{FileOrURL},data);
  }
  
  public Body(World world,String FileOrURL) {
    //this(world,toStrings(FileOrURL),data);
    this(world,new String[]{FileOrURL},FileOrURL);
  }

  final private HashMap transforms;
  
  private TransformGroup focus;
  
  public boolean isAlive() {
    return !(transforms.isEmpty());
  }
 
  public void clearTransforms() {
    transforms.clear();
  }
  
  public boolean isFocused() {
    return null!=this.focus;
  }
  
  /**
     Sets the focus on a given part. As a result,
     tranfromations applied to thgis agent will affect
     the given part. By setting the focus to null, the
     agent as a whole becomes the focus of the transforms -
     which is, in fact, the default behavior inherited
     from its superclass.     
  */
  
  public TransformGroup getFocus() {
    return this.focus;
  }
  
  synchronized public boolean setFocus(String name) {
    //Prolog3D.pp("focus to:"+name);
    if(null==name) {
      this.focus=null;
      initFromTG(getGroup());
    }
    else {
      Object val=transforms.get(name);
      if(val instanceof TransformGroup) {
        this.focus=(TransformGroup)val;
        initFromTG(this.focus);
       }
       else
         this.focus=null;
    }
    sleep();     
    return null!=this.focus;
  }
  
  /**
     Overrides transformation step to apply to
     current focus.
   */
  public void transformFocus() {
    //Prolog3D.pp(this.data+"=>focus="+focus);
    if(null==this.focus) super.transformFocus();
    else applyTransformTo(focus);
  }
    
  public void runAuto() {
    //Prolog3D.pp(this.data+".focus="+this.focus);
    
    Iterator I=this.transforms.keySet().iterator();
    int l=transforms.size();
    if(0==l) {
      super.runAuto();
      return;
    }
    int n=Params.ri(l);
    int i=0;
    while(I.hasNext()) {
      String K=(String)I.next();
      if(n==i++) {
        //Prolog3D.pp(this.data+"=>rotating: "+K);
        setFocus(K);
        rotAuto();
        break;
      }
    }
  }
  
  private void rotAuto() {
    //Prolog3D.pp(this.data+".focus="+this.focus);
    int choice=Params.ri(3);
    double d=rf(0.5);
    switch(choice) {
      case 0:
        setRotX(d);
        break;
      case 1:
        setRotY(d);
        break;
      case 2:
        setRotZ(d);
        break;
    }
  }
  
  public String toString() {
    return data+":"+transforms;
  }
}

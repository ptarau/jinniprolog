package prolog3d;
import prolog.kernel.JavaIO; //?!
import prolog.logic.Fun;
import javax.media.j3d.*;
import javax.vecmath.*;
import java.util.*;
import java.io.*;
import java.beans.XMLDecoder;
import com.sun.j3d.loaders.objectfile.ObjectFile;
import com.sun.j3d.loaders.ParsingErrorException;
import com.sun.j3d.loaders.IncorrectFormatException;
import com.sun.j3d.loaders.Scene;
import com.sun.j3d.utils.scenegraph.io.*;
import java.util.zip.*;

  // added !!!
//import com.sun.j3d.loaders.vrml97.VrmlLoader;

import org.jdesktop.j3d.loaders.vrml97.VrmlLoader;

/**
   Imports/exports various formats: VRML, OBJ, XML, j3f
   from/to URLs and files which can be gzipped for most formats.
 */
public class Convert {  

  Convert() {
  }
    
  static public String fixFileName(String fname) {
    if(fname.startsWith("http://")) return fname;
    
    if(!Params.isApplet()) {
      if(new File(fname).exists()) return fname;
    }
    else {
      try {
        //Prolog3D.pp("TRYING fname="+fname);
        fname=JavaIO.getPrologHome()+fname;
        return fname;
      }
      catch(Exception e) {
      }
    }
    return null;
  }

  static public InputStream urlInput(String url) {
    try {
      url=fixFileName(url);
      InputStream s=new BufferedInputStream(JavaIO.url_or_file(url));
      if(url.endsWith(".gz")) s=new GZIPInputStream(s);
      return s;
    }
    catch(Exception e) {
      Prolog3D.pp("error opening: "+url+"=>"+e);
      return null;
    }
  }
  
  static public OutputStream fileOutput(String fname) {
    try {
      OutputStream s=new BufferedOutputStream(new FileOutputStream(new File(fname)));
      if(fname.endsWith(".gz")) s=new GZIPOutputStream(s);
      return s;
    }
    catch(Exception e) {
      Prolog3D.pp("error opening: "+fname+"=>"+e);
      return null;
    }
  }
  

  /**
     loads files in various formats to BranchGroup objects
   */
  public static BranchGroup file2bgroup(String filename) {
    if(filename.endsWith(".j3f") || filename.endsWith(".J3F") 
    || filename.endsWith(".j3f.gz") || filename.endsWith(".J3F.GZ")) 
      return jf2bgroup(filename);  
    //else  
    Scene s=file2scene(filename);
    if(null==s) return null;
    BranchGroup b =  s.getSceneGroup();
    Hashtable H=s.getNamedObjects();
    if(null!=H) {
      HashMap HM=new HashMap(H);
      //Prolog3D.pp("names="+HM);
      fixMap(HM);
      b.setUserData(HM);
    }
    return b;
  }
  
  public static void fixMap(HashMap H) {
    Iterator I=H.entrySet().iterator();
    while(I.hasNext()) {
      Map.Entry E=(Map.Entry)I.next();
      if(!(E.getValue() instanceof TransformGroup )) {
        I.remove();
        //Prolog3D.pp("removing="+E+"=>"+E.getValue().getClass().getName());
      }
      else {
        TransformGroup TG=(TransformGroup)E.getValue();
        TG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        TG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
      }
    }
  }
  
  /**
   loads files in various formats to BranchGroup objects trying them one by one
   */
  public static BranchGroup file2bgroup(String[] fs) {
    for(int i=0;i<fs.length;i++) {
      BranchGroup b=file2bgroup(fs[i]);
      if(null==b) continue;
      //Prolog3D.pp("humanoid found at: "+fs[i]);
      return b;
    }
    return null;
  }
  
  private static Scene file2scene(String filename) {
    Scene s=null;
    if(filename.endsWith(".obj") || filename.endsWith(".OBJ")
      || filename.endsWith(".obj.gz") || filename.endsWith(".OBJ.GZ"))
      s=obj2scene(filename);
    else if(filename.endsWith(".wrl") || filename.endsWith(".WRL")
      || filename.endsWith(".wrl.gz") || filename.endsWith(".WRL.GZ"))
      s=vrml2scene(filename);
    else 
      Prolog3D.pp("*** no loader for: " + filename);
    return s;
  }

  
  public static void file2jf(String f,String jf) {
    BranchGroup b=file2bgroup(f);
    bgroup2jf(b,jf);
  }
  
  /**
     Saves a BranchGroup to a j3f.gz file.
   */ 
  public static void bgroup2jf(BranchGroup b,String jf) {
    try {
      OutputStream s=fileOutput(jf);
      if(null==s) return;
      SceneGraphStreamWriter w;
      w=new SceneGraphStreamWriter(s);
      HashMap H;
      Object D=b.getUserData();
      if(null!=D && D instanceof HashMap) {
        H=(HashMap)D;      
        b.setUserData(null);
      }
      else H=new HashMap();
      Iterator I=H.keySet().iterator();
      while(I.hasNext()) {
        Object k=I.next();
        if(b==k) continue;
        Node n=(Node)H.get(k);
        if(null==n) continue;
        n.setUserData(k);
      }
      w.writeBranchGraph(b,H);
      //Prolog3D.pp("Saving HashMap="+H+"\n");
      w.close();
      s.close();
    }
    catch(Exception e) {
      e.printStackTrace();
    }
  }
  
  /**
   Loads a j3f or j3f.gz file to a BranchGroup
   */
  public static BranchGroup jf2bgroup(String jf) {  
    try {
      InputStream s=urlInput(jf);
      if(null==s) return null;
      SceneGraphStreamReader r=new SceneGraphStreamReader(s);
      HashMap H=new HashMap();
      BranchGroup b=r.readBranchGraph(H);
      b.setUserData(H);
      Tools.collectChildren(b); // fixes HashMap in b - a Sun bug - which has no proper nodes attached to names
      //Prolog3D.pp("Loading HashMap="+H+"\n");
      s.close();
      return b;
    }
    catch(Exception e) {
      e.printStackTrace();
      return null;
    }
  }
  
  /**
     Loads a .wrl or .wrl.gz VRML file to a Scene
   */
  public static Scene vrml2scene(String filename) {
    Scene s = null;
    VrmlLoader f = new VrmlLoader();
    try {
      filename=Convert.fixFileName(filename);
      if(filename.startsWith("http://"))
        s=f.load(new java.net.URL(filename));
      else
        s = f.load(filename);
    }
    catch (Exception e) {
      Prolog3D.pp(e);
      return null;
    }
    return s;
  }
   
  /**
     Loads a Shape3D serialized to XML using XML encoding for Java Beans
   */  
  static public Shape3D xml2shape(String fname) {
    try {
      XMLDecoder d = new XMLDecoder(urlInput(fname));
      Object result = d.readObject();
      d.close();
      return (Shape3D)result;
    }
    catch(Exception e) {
      e.printStackTrace();
    }
    return null;
  }
  
  /**
     Loads an array of Shape3D objects for use in a Morph
   */
  public static Shape3D[] xml2shapes(Fun names) {
    Object[] args=names.args;
    int l=args.length;
    Shape3D[] shapes=new Shape3D[l];
    for(int i=0;i<l;i++) {
      shapes[i]=Convert.xml2shape(args[i].toString());
    }
    return shapes;
  }
  
  private static final boolean noTriangulate = false;
  private static final boolean noStripify = false;
  private static final double creaseAngle = 60.0;

  /**
     Loads a Wawefront obj or obj.gz file to a Scene
  */
  public static Scene obj2scene(String filename) {
    int flags = ObjectFile.RESIZE;
    if (!noTriangulate) flags |= ObjectFile.TRIANGULATE;
    if (!noStripify) flags |= ObjectFile.STRIPIFY;
      
    ObjectFile f =
      new ObjectFile(flags, 
      (float)(creaseAngle * Math.PI / 180.0));
    Scene s = null;
    try {
      filename=fixFileName(filename);
      if(filename.startsWith("http://"))
        s=f.load(new java.net.URL(filename));
      else
        s = f.load(filename);
    }
    catch (Exception e) {
      Prolog3D.pp(e);
      return null;
    }
    return s;
  }
}

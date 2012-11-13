package prolog3d;

import prolog.core.Cat;

import javax.media.j3d.*;
import javax.vecmath.*;
import java.util.*;

public class Tools {  

  public Tools() {
  }
  
  public static String showChildren(Node N) {
    StringBuffer b=new StringBuffer();
    showChildren(N,b,0);
    return b.toString();  
  }
  
   public static void showChildren(Node N,StringBuffer b,int d) {
    for(int i=0;i<d;i++) {
      b.append(' ');
    }
    Object D=N.getUserData();  if(null!=D) b.append(D+":");
    if(N instanceof Link) {
      b.append(N+"=>\n");
      N=((Link)N).getSharedGroup();
      showChildren(N,b,d+1);
    }
    else if(N instanceof Leaf) {
       b.append(N+"\n");
    }
    else {
      Group G=(Group)N;
      b.append(showGroup(G)+"\n");
      //G.setCapability(Group.ALLOW_CHILDREN_READ);
      Enumeration e=G.getAllChildren();
      while(e.hasMoreElements()) {
        Node C=(Node)e.nextElement();
        showChildren(C,b,d+1);
        //if(e.hasMoreElements()) b.append("\n");
      }
    }
  }
  
  public static String showGroup(Group G) {
    if(G instanceof TransformGroup) {
      TransformGroup TG=(TransformGroup)G;
      return G.toString();
    }
    return G.toString();
  }

  public static Cat collectChildren(Node N) {
    Cat cat=new Cat();
    Object O=N.getUserData();
    HashMap H;
    if(O instanceof HashMap) H=(HashMap)O;
    else H=null;
    if(null==H) H=new HashMap();
    collectChildren(N,cat,0,H);
    N.setUserData(H);   
    return cat;  
  }
  
  public static void collectChildren(Node N,Cat cat,int d,HashMap H) {
   
    if(N instanceof Link) {
      cat.setProp(N,"link",new Integer(d));
      Node G=((Link)N).getSharedGroup();
      collectChildren(G,cat,d+1,H);
      cat.setMorphism(N,G,"child","link");
    }
    else if(N instanceof Leaf) {
      cat.setProp(N,"leaf",new Integer(d));
    }
    else {
      Group G=(Group)N;
      cat.setProp(G,"group",new Integer(d));
      collectGroup(G,cat,d);
      Enumeration e=G.getAllChildren();
      while(e.hasMoreElements()) {
        Node C=(Node)e.nextElement();
        collectChildren(C,cat,d+1,H);
      }
    }
    Node P=N.getParent();
    if(null!=P) cat.setMorphism(P,N,"child","group");
    fixUserData(N,H);
  }
  
  public static void fixUserData(Node N,HashMap H) {
    Object key=N.getUserData();
    if(null==key) return;
    if(key instanceof String) {
      H.put(key,N);
    }
  }
  
  public static void collectGroup(Group G,Cat cat,int d) {
    if(G instanceof TransformGroup) {
      TransformGroup TG=(TransformGroup)G;
      TG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
      TG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
     
      /*
      Transform3D T=new Transform3D();
      // some transfor tests go here
      TG.getTransform(T);
      cat.setProp(T,"transform",new Integer(d));
      cat.setMorphism(T,TG,"parent","transform");
      */
    }
  }
  
  public static void drawChildren(Group G) {
    Cat C=collectChildren(G);
    //Prolog3D.pp(Cat.showInfo(C));
    Prolog3D.drawGraph(C);
  }
}

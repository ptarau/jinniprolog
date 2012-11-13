package prolog3d;

//import javax.media.j3d.*;
import javax.vecmath.*;

import prolog.core.Cat;
import prolog.core.IVertex;
import prolog.core.IEdge;
import prolog.core.RankedData;

import java.util.*;

public class TreeLayout extends LayoutEngine {

  public TreeLayout(Prolog3D jworld,Cat RG,int radius,boolean addVertex) {
    super(jworld,RG,radius,addVertex);
  }

  boolean isRoot(IVertex V) {
    return 0==V.inLinks.length;
  }

  boolean isLeaf(IVertex V) {
    return 0==V.outLinks.length;
  }
  
  boolean isInternal(IVertex V) {
    return !(isLeaf(V) || isRoot(V));
  }

  public int setShape(RankedData RD,IVertex V) {
    int shape;
    if(isRoot(V)) shape=Shape.TETRA; // root
    else if(isLeaf(V)) shape=Shape.CUBE; // leaf
    else shape=Shape.SPHERE; // internal node
    return shape;
  }

  public void initPosition(IVertex V,MobilePoint N) {
    Point3f RP;
    if(isRoot(V)) {
      RP=new Point3f(0,0,0);
      N.fixed=true;
    }
    else if(isLeaf(V)) {
      RP=Params.rs(radius);
      N.fixed=true;
    }
    else {
      RP=Params.rs(radius*0.50f);
    }
    N.setX((double)RP.x);
    N.setY((double)RP.y);
    N.setZ((double)RP.z);

    //running=false;
  }

  /** 
   O(E) spring elasticity on edges
   */
  public void edgeFun(int i0,MobilePoint n,IEdge e,int i) {
    //IVertex V0=vertices[i0];
    //IVertex V=vertices[i];
    MobilePoint t=getPoint(e.to);
    double vx = t.getX()-n.getX();
    double vy = t.getY()-n.getY();
    double vz = t.getZ()-n.getZ();
    double len = Math.sqrt(vx*vx + vy*vy + vz*vz);
    double f = (edgeLength-len)/(radius+i);
    double dx = f * vx;
    double dy = f * vy;
    double dz = f * vz;
    if(!t.fixed) {
      t.dx += dx;
      t.dy += dy;
      t.dz += dz;
    }
    if(!n.fixed) {
      n.dx += -dx;
      n.dy += -dy;
      n.dz += -dz;
    }
  }
  
  /**
    O(N^2) action on vertices
   */
 public  void vertexStep(int i,MobilePoint n) {
   //if(true) return;

   //if(!isInternal(vertices[i])) return;
  
   if(n.fixed) return;
  
   DataPoint P=new DataPoint();
 
    if(true) {
      double nx=n.getX();
      double ny=n.getY();
      double nz=n.getZ();
      double nr=Math.sqrt(nx*nx+ny*ny+nz*nz);     
      P.dx = nx/(radius*nr);
      P.dy = ny/(radius*nr);
      P.dz = nz/(radius*nr);
    }
      
    for (int j = 0 ; j < size() ; j++) {
      if (i == j) {
        continue;
      }
      //if(! isInternal(vertices[j])) continue;
      MobilePoint n2 = getPoint(j);
      if(n2.fixed) 
        continue;

      PointFun(P,n,n2);
    }
      
    double dlen = P.dx * P.dx + P.dy * P.dy + P.dz * P.dz;
    if (dlen > 0) {
      dlen = Math.sqrt(dlen/radius); //$$
      n.dx += P.dx / dlen;
      n.dy += P.dy / dlen;
      n.dz += P.dz / dlen;
    }
  }
  
 public void PointFun(DataPoint P,MobilePoint n1,MobilePoint n2) {   
    double vx = n1.getX() - n2.getX();
    double vy = n1.getY() - n2.getY();
    double vz = n1.getZ() - n2.getZ();
    double len = vx * vx + vy * vy;       
    if (len == 0) {
      P.dx += Params.rf();
      P.dy += Params.rf();
      P.dz += Params.rf();
    } else if (len < radius*radius) {
      double f=n1.rank;
      len=len/Math.sqrt(f);
      P.dx += vx / len;
      P.dy += vy / len;
      P.dz += vz / len;
    }
  }
}

package prolog3d;
import prolog.core.IEdge;
import prolog.core.IVertex;
import prolog.core.IGraph;
import prolog.core.Cat;
import prolog.core.RankedData; //?!

import javax.media.j3d.*;
import javax.vecmath.*;

abstract public class LayoutEngine extends IGraph {
  private final Prolog3D jworld;
  protected int radius;
  int edgeLength;
  
  public boolean running;
  
  public LayoutEngine(Prolog3D jworld,Cat RG,int radius,boolean addVertex) {
    super(RG,RG.getIFilter()); // passing Cat's IFilter
    this.jworld=jworld;
    this.radius=radius;
    getCenter();
    //R.setSeed(99);
    running=true;
    //boolean addVertex=!(RG instanceof Cat);
    addGraph(RG,addVertex);
    //Prolog3D.pp("!here: "+RG);
  }

  public int setShape(RankedData RD,IVertex V) {
    int shape;
    switch(RD.hyper) {
      case 1:shape=Shape.CUBE; break;
      case 2:shape=Shape.POINT;break;
      case 3:shape=Shape.TETRA;break;
      case 4:shape=Shape.CCUBE; break;
      case 5:shape=Shape.TEXT2D;break;
      case 6:shape=Shape.TEXT3D;break;
      default: shape=Shape.SPHERE; // also 0
    }
    return shape;
  }

  public void initPosition(IVertex V,MobilePoint N) {
    Point3f RP=Params.rs(0.75f*radius);
    N.setX((double)RP.x);
    N.setY((double)RP.y);
    N.setZ((double)RP.z);
  }

  public void addGraph(Cat RG,boolean add) {
    for(int i=0;i<size();i++) {
      IVertex V=vertices[i];
   
      RankedData RD=(RankedData)RG.rawVertexData(V.key);
      //Object RS=RG.getSelected(V.key);
      Object RS=getIFilter().filterVertex(RD);
      // V = key + data
      Vertex3D GV;

      int shape=setShape(RD,V);
      Color3f color=comp2col(RD.component,RD.hyper);
  
      if(add) {
        if(shape<Shape.maxSimple)
          GV=jworld.addVertex(shape,color,V);
        else {
          Shape dataShape=new Shape(color);
          GV=jworld.addVertex(dataShape.toNode(shape,RS),RS); //$$
        }
      }
      else GV=(Vertex3D)(V.key);
      
      MobilePoint N=new MobilePoint(RS,RD,GV,radius,size());
      V.data=N; // V contains N && N contains V - careful with toString!!!

      initPosition(V,N);
    }
    for(int i=0;i<size();i++) {
      IVertex F=vertices[i];
     
      MobilePoint MFrom=(MobilePoint)(F.data);
      Vertex3D From=MFrom.V;
       
      IEdge[] es=F.outLinks;
      for(int j=0;j<es.length;j++) {
        IEdge E=es[j];
        IVertex T=vertices[E.to];
        MobilePoint MTo=(MobilePoint)(T.data);
        Vertex3D To=MTo.V;
        if(add) E.data=jworld.addEdge(From,To,E.data); //$$
      }
    }
  }
   
  public void refresh(int radius) {
    this.radius=radius;
    getCenter();
  }
  
  public void getCenter() {
    edgeLength=(int)(Math.PI*radius/size());
    edgeLength=Math.min(140,Math.max(100,edgeLength));
    //pp("edgeLength="+edgeLength+" rad="+radius);
  }
  
  
  static public void pp(Object O) {
    Prolog3D.pp(O.toString());
  }
    
  MobilePoint getPoint(int i) {
    return (MobilePoint)vertices[i].data;
  }
  
  public static Color3f comp2col(int c,int hyper) {
    c=Math.max(0,c);
    float red=(hyper>0)?0:(float)(1/(1+Math.log(1+c)));
    float green=1-red;
    float blue=0;
    //Prolog3D.pp("ranked color: c="+c+".h="+hyper+",r="+red+",g="+green+",b="+blue);
    Color3f col=new Color3f(red,green,blue);
    return col;
  }

  /** 
   O(E) spring elasticity on edges
   */
  abstract public void edgeFun(int i,MobilePoint n,IEdge e,int j);
  
  /**
   O(N^2) action on vertices
   */
  abstract public void vertexStep(int i,MobilePoint n);
  
  abstract public void PointFun(DataPoint P,MobilePoint n1,MobilePoint n2);
  
  void edgeStep(int i,MobilePoint n,IEdge[] es) {
    for(int j=0; j<es.length; j++) {
      if(!running) break;
      edgeFun(i,n,es[j],j);
    }    
  }
  
  boolean trace=false;
  boolean isStable() {
    if(trace)pp("oldenergy="+oldenergy);
    stepCount++;
    if(trace)pp("energy="+energy);
    double dynamism=Math.abs(energy-oldenergy)/oldenergy-0.001*Math.log(size());
    if(trace)pp("dynamism="+dynamism);
    long smallsize=size();
    if(smallsize>200) smallsize=200;
    double youth=2*smallsize*Math.log(size())-stepCount;
    if(trace)pp("youth="+youth);
    if(trace)pp("");
    if(!running) return true; 
    return dynamism<0 && youth<0;
  }
  
  long stepCount=0;
  //int sleepCount=0;
  
  long getSleepTime() {
    return Params.layoutSleep;
  }

  
  final static int maxd=5;
  
  final public double moderate(double d) {
    if(d<-maxd) d=-maxd;
    else if(d>maxd) d=maxd;
    return d;
  }
  
  double applyDelta(MobilePoint n,double energy) {
    if(!n.fixed) {
      n.dx /= 2;n.dy /= 2;n.dz /= 2;
      double dx = moderate(n.dx);
      double dy = moderate(n.dy);
      double dz = moderate(n.dz);
      double x=n.getX();
      double y=n.getY();
      double z=n.getZ();
      n.incX(dx);
      n.incY(dy);
      n.incZ(dz);
      if(!borderFun(n)) {n.setX(x);n.setY(y);n.setZ(y);n.dx=0;n.dy=0;n.dz=0;}
      energy+=(dx*dx+dy*dy+dz*dz);
    }
    return energy;
  }
  
  public boolean borderFun(MobilePoint n) {  
    float d=radius-50f;
    double x=n.getX();
    double y=n.getY();
    double z=n.getZ();
    double l=d;
    double max=10.0;
    double m=1/max;
    if (!n.fixed) {
      int i=0;
      for(;i<max;i++) {
        l=Math.sqrt(x*x+y*y+z*z);
        if(l>d) {
          x-=x*m;
          y-=y*m;
          z-=z*m;
        }
        else
          break;
      }
      if(i>0) {
        n.setX(x);
        n.setY(y);
        n.setZ(z);
        n.dx=0;
        n.dy=0;
        n.dz=0;
      }
    }
    return true;
  }

  public void stop() {
    Prolog3D.pp("layout stopping");
    running=false;
  }

  double energy=0;
  double oldenergy=size();
  
  /**
   main O(N^2+E) iteration step
   */
  synchronized void step() {
    if(Params.stopAll) running=false;
    if(energy>0) oldenergy=energy;
    energy=0;
    for (int i=0;i<size();i++) {
      if(!running) break;
      IVertex V=vertices[i];
      MobilePoint n = getPoint(i);
      vertexStep(i,n);    
      edgeStep(i,n,V.outLinks);
      energy=applyDelta(n,energy);
    }
    energy=energy/(size()*size());   
    if(isStable()) {
      if(stepCount>0) {
        double st=Math.log(stepCount);
        double si=Math.log(size());
        //pp("steps="+st);
        //pp("si="+si);
        pp("size="+size()+", steps="+stepCount+
          ", O(N^"+ (1+st/si)+ ")");
      }
      running=false;
    }
  }
  
  public void run() {
    for(;;) {
      step();
      if(!running) break;
      if(Params.layoutSleep>0) {
        Params.sleep(getSleepTime());
      }  
    }
    if(Params.layoutSleep==0) Params.sleep(1000);
    Prolog3D.pp("layout finished");
  }
  
}


package prolog3d;

import javax.media.j3d.*;
import javax.vecmath.*;
import prolog.core.IEdge; //?!
import prolog.core.IVertex;

/**
A Point/Line representation of Graph as an efficiently
represented single object.
 */
public class FrozenGraph extends Point {

  static Color3f defPointCol=new Color3f(1.0f,1.0f,0.0f);
 
  static Color3f defFromCol=new Color3f(1.0f,1.0f,1.0f);
  static Color3f defToCol=new Color3f(0.0f,0.0f,0.0f);
 
  /**
   Generates a random Frozen Graph
  */
  public FrozenGraph() {
    super();
  }
  
   
  /**
   Generates a random Frozen Graph
  */
  public static FrozenGraph random() {
    return random(100,200);
  }  
    
  public static FrozenGraph random(int nv,int ne) { 
    FrozenGraph G=new FrozenGraph();
    G.beginGraph(nv,ne);
      
    for(int i=0; i<G.vertices.length; i++) {        
      G.setVertex(i,Params.rs(0.5f));
    } 
    
    for(int i=0; i<G.edges.length/2; i++) {
      G.setEdge(i,Params.ri(G.vertices.length),Params.ri(G.vertices.length));  
    } 
    
    G.endGraph();
    
    return G;
  }
  
  
  /*
  public FrozenGraph(int nVertices,int nEdges) {
    super();
    
   // beginGraph(nVertices,nEdges);
     
    // DO for each vertex and edge:
       
    //setVertex(i,x,y,z );
    //setEdge(i,from,to);  
    
    //endGraph();
  }
  */
 
  private Point3f[] vertices;
  private Color3f[] vcolors;
  private Point3f[] edges;
   
  public void beginGraph(int vertexCount,int edgeCount) {
    vertices=new Point3f[vertexCount];
    vcolors=new Color3f[vertexCount];
    edges=new Point3f[2*edgeCount];  
  }
 
  public void setVertex(int i,Point3f v) {
    this.vertices[i]=v;
  }
    
  public void setVertex(int i,float x,float y,float z) {
    this.vertices[i]=new Point3f(x,y,z);
  }
  
  public void colorVertex(int i,float R,float G,float B) {
    this.vcolors[i]=new Color3f(R,G,B);
  }
  public void setEdge(int i,int from,int to) {
    this.edges[2*i] = this.vertices[to];
    this.edges[2*i+1] = this.vertices[from];
  }
  
  public void endGraph(Color3f pointColor) {
    for(int i=0; i<vcolors.length; i++) {
      if(null==vcolors[i]) vcolors[i]=pointColor;
    }
    addChild(new FrozenPointShape(vertices,vcolors));
    addChild(new FrozenLineShape(edges));
  }
  
  public void endGraph() {
    endGraph(defPointCol);
  }
  
  static Point3f mp2p(MobilePoint P,float r) {
    return new Point3f((float)P.getX()/r,(float)P.getY()/r,(float)P.getZ()/r);
  }
  
  /**
     Builds a FrozenGraph from the results of a layout engine
   */
  public FrozenGraph(LayoutEngine LG) {
    super();
    
    IVertex[] vs=LG.vertices;
    int vsL=vs.length; // total vertices: vsL
    // compute total edges: esL
    int esL=0;
    for(int i=0;i<vsL;i++) {
      IVertex F=vs[i];
      IEdge[] es=F.outLinks;
      esL+=es.length;
    }
    
    if(esL==0) return; // no edges - exception
    
    // allocate vertices, edges
    beginGraph(vsL,esL);
    
    // add vertices
    for(int i=0;i<vsL;i++) {
      IVertex F=vs[i];
      MobilePoint P=(MobilePoint)F.data;
      setVertex(i,mp2p(P,2*LG.radius));
    }
    
    // add edges
    int eCount=0;
    for(int i=0;i<vsL;i++) {
      IVertex F=vs[i];
      IEdge[] es=F.outLinks;
      for(int j=0;j<es.length;j++) {
        IEdge E=es[j];
        setEdge(eCount++,i,E.to);   
      }
    }
    //ain3D.pp("eCount="+eCount+",edges="+esL);
    // build object
    endGraph();   
  }   

}

/**
   The shape supporting 
 */
class FrozenPointShape extends Shape3D {

  public FrozenPointShape(Point3f[] vertices,Color3f[] colors) {
    FrozenPointGeom points=new FrozenPointGeom(vertices,colors);
    this.setGeometry(points);
    this.setAppearance(Simple.defApp);
  }
}

class FrozenLineShape extends Shape3D {
  public FrozenLineShape(Point3f[] edges) {
    FrozenLineGeom wires=new FrozenLineGeom(edges,FrozenGraph.defFromCol,FrozenGraph.defToCol);
    this.setGeometry(wires);
    this.setAppearance(Simple.defApp);
  }
}

class FrozenLineGeom extends LineArray {
  
  FrozenLineGeom(Point3f[] edges, Color3f fromColor, Color3f toColor) {
    super(2*edges.length, GeometryArray.COORDINATES | GeometryArray.COLOR_3);  
    setCoordinates(0, edges);     
    Color3f[] colors = new Color3f[2*edges.length];
    for(int i=0; i<edges.length; i++) {
      colors[2*i] = fromColor;
      colors[2*i+1] = toColor;
    }
    
    setColors(0, colors);    
  }
}

class FrozenPointGeom extends PointArray { 
  FrozenPointGeom(Point3f[] vertices, Color3f[] colors) {
    super(vertices.length, GeometryArray.COORDINATES | GeometryArray.COLOR_3);  
    setCoordinates(0, vertices);  
    setColors(0,colors);    
  }
  
}


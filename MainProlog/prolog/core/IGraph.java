package prolog.core;
import prolog.kernel.*;
import prolog.logic.*;

public class IGraph implements Stateful {
  public IGraph(int size) {
    vertices=new IVertex[size];
  }
  
/*  public IGraph(Graph G) {
    this(G,null);
  }
*/  
  public IGraph(Graph G,IFilter iFilter) {
    this.iFilter=iFilter;
    vertices=new IVertex[G.size()];
    ObjectIterator Vs=G.vertexIterator();
    while(Vs.hasNext()) {
      Object V=Vs.next();
      Object selData=G.rawVertexData(V);
      //Object selData=G.getSelected(V);
      
      if(null==selData) JavaIO.warnmes(
        "bad data in IGraph("+V+"):<<<"+G.rawVertexData(V)+">>>"
      );
          
      IVertex X=new IVertex(V,selData,G.inDegree(V),G.outDegree(V));
      vertices[G.getOrdinal(V)]=X;
      {
        ObjectIterator Is=G.inIterator(V);
        int e=0;
        while(Is.hasNext()) {
          Object I=Is.next();
          Object D=G.edgeData(V,I);
          int i=G.getOrdinal(I);
          IEdge iE=new IEdge(i,D);
          X.inLinks[e++]=iE;
        }
      }
      {
        ObjectIterator Os=G.outIterator(V);
        int e=0;
        while(Os.hasNext()) {
          Object O=Os.next();
          Object D=G.edgeData(V,O);
          int o=G.getOrdinal(O);   
          IEdge iE=new IEdge(o,D);
          X.outLinks[e++]=iE;
        }
      }    
    }
  }
  
  private IFilter iFilter;
  
  public void setIFilter(IFilter iFilter) {
    this.iFilter=iFilter;
  }
 
  public IFilter getIFilter() {
    return this.iFilter;
  }
   
  public IVertex[] vertices;
  
  public final int size() {
    return vertices.length;
  }
  
  public String toString() {
    StringBuffer buf=new StringBuffer();
    for(int i=0;i<vertices.length;i++) {
      IVertex k=vertices[i];
      buf.append("["+i+"]");
      String z=k.toString();
      if(null!=z) {
        buf.append(":");
        buf.append(z); 
      }
      buf.append(""); 
      IEdge[] es=k.outLinks;
      if(es!=null && es.length>0) {
        buf.append("=>");
        for(int j=0;j<es.length;j++) {
          if(j>0) buf.append(",");
          IEdge iE=es[j];
          buf.append("["+iE.to+"]");
          String s=es[j].toString();
          if(null!=s) {
            buf.append(":");
            buf.append(s);
          }
        }
      }
      buf.append("\n");
    }
    buf.append("\n");
    return buf.toString();
  }

}
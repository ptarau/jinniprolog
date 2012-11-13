package prolog.core;
import prolog.kernel.*;
import prolog.logic.*;

public final class IVertex implements Stateful {
  public Object key;
  public Object data;
  public final IEdge[] inLinks;
  public final IEdge[] outLinks;

  public IVertex(Object key, Object data, int inDegree, int outDegree) {
    this.key = key;
    this.data = data;
    this.inLinks = new IEdge[inDegree];
    this.outLinks = new IEdge[outDegree];
  }

  public String toString() {
    return key + "->" + data;
  }
}

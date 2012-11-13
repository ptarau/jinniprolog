package prolog.core;
import prolog.kernel.*;
import prolog.logic.*;

public class ComponentCollector implements GraphVisitor {
  ComponentCollector(RankedGraph RG) {
    this.RG=RG;
  }
  
  private int componentCount;
  private IntStack collected;
  private int elementCount;
  
  private RankedGraph RG;
  
  public void init() {
    componentCount=-1;
    elementCount=-1;
    this.collected=new IntStack();
  }
  public void start() {
    componentCount++;
    elementCount=0;
  }
  
  public void stop() {
    collected.push(elementCount);
  };
  
  public Object end() {
    RG=null;
    componentCount++;
    return collected;
  }
  
  public boolean isVisited(Object V) {
    return RG.getComponent(V)>=0;
  }
  
  public boolean visitIn() {return true;}
  
  public boolean visitOut() {return true;}
  
  public void visit(Object V) {
    elementCount++;
    RG.setComponent(V,componentCount);
  }

  public void unvisit(Object V) {
  }
}

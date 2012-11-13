package prolog.core;
import prolog.kernel.*;
import prolog.logic.*;

 public interface IFilter extends Stateful {
   public Object filterVertex(Object VData);
   public Object filterEdge(Object EData);
 }

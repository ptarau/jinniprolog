package prolog.core;
import prolog.kernel.*;
import prolog.logic.*;
public interface CatWalker extends Stateful {
  public void atStart();

  public void beforeProps();
  public void onProp(Object vertex,Object key,Object value);
  public void afterProps();

  public void beforeMorphisms();
  public void onMorphism(Object from,Object to,Object m,Object md);
  public void afterMorphisms();

  public Object atEnd();
}

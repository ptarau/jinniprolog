package prolog.core;
import prolog.kernel.*;
import prolog.logic.*;

/**
Implements (Finite) Categories by extending Graph to support multiple
edges (seen as morphisms) between vertices (seen as a Category's Objects)
as well as some basic Category theory operations. Uses BitVectors to
represent a small set of morphisms. It can be made to extend either
Graph or Cat. We chose to extend Cat so that it can provide a small
set of morphisms sharing attributes more efficiently, while allowing
also the use of an unbound number of general morphisms with individual
attributes - as inherited from Cat. Uses BitSets through RoleMap class.
*/

public class SmallCat extends RankedGraph { // Graph

  /**
  Creates a new Small Category (supporting fast
  processing for a small number of morphisms, known in advance - 
  typical situation in a semantic network)
   */
  SmallCat() {
    super();
    morphisms = new ObjectDict();
  }

  SmallCat(int size) {
    super(size);
    morphisms = new ObjectDict();
  }

  final private ObjectDict morphisms;


  /* do not do this - it might ruin subclasses - the right thing
   not to inherit but explicitely delegate to Graph !!!
   */

  public void addVertex(Object v, Object data) {
    JavaIO.errmes("error directly adding vertex in SmallCat:" + v + ":" + data);
  }

  public void addEdge(Object from, Object to, Object data) {
    JavaIO.errmes("error directly adding edge in SmallCat:" + from + "=>" + to + ":" + data);
  }

  /**
  Defines a morphism and attaches to it a fixed attribute.
   */
  public void defineSmallMorphism(Object name, Object info) {
    morphisms.put(name, info);
  }

  /**
   states that there's a morphism m between two objects
   */
  public void setSmallMorphism(Object from, Object to, Object m) {
    RoleMap D = (RoleMap)edgeData(from, to);
    if (null == D) {
      D = new RoleMap(morphisms);
      super.addEdge(from, to, D);
    }
    D.set(m);
  }

  /**
  checks if there's a morphism m between two objects
   */
  public boolean isSmallMorphism(Object from, Object to, Object m) {
    RoleMap D = (RoleMap)edgeData(from, to);
    if (null == D) return false;
    return D.get(m);
  }

  /**
    returns the attribute of a morphism m if it exists between two objects
   */
  public Object getSmallMorphism(Object from, Object to, Object m) {
    if (!isSmallMorphism(from, to, m)) return null;
    return morphisms.get(m);
  }

  public void addSmallMorphism(Object from, Object to, Object m) {
    if (null == morphisms.get(m)) defineSmallMorphism(m, m);
    setSmallMorphism(from, to, m);
  }

  /**
  removes a morphism m between two objects
   */
  public void clearSmallMorphism(Object from, Object to, Object m) {
    RoleMap D = (RoleMap)edgeData(from, to);
    if (null != D) {
      D.clear(m);
    }
  }

  public static void sctest() {
    try {
      SmallCat C = new SmallCat();

      C.defineSmallMorphism("hyp", "is a kind of");
      C.defineSmallMorphism("mm", "is part of");
      C.setSmallMorphism("weel", "car", "mm");
      C.setSmallMorphism("car", "vehicle", "hyp");
      C.setSmallMorphism("vehicle", "thing", "hyp");
      C.setSmallMorphism("car", "engine", "mm");
      C.setSmallMorphism("engine", "thing", "hyp");
      Object m = C.getSmallMorphism("car", "vehicle", "hyp");
      Prolog.dump("got car / vehicle =>" + m);
      //C.setProp("car","brand","Toyota");
      C.runGraphRanker();
      C.rankSort();
      //C.addEdge("car","vehicle","is_a,relation");
      Prolog.dump("cat=\n" + C);
    }
    catch (Exception e) {
      JavaIO.printStackTrace(e);
    }
  }
}
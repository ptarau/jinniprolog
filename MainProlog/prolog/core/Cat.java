package prolog.core;
import prolog.kernel.*;
import prolog.logic.*;
import java.util.Random;

/**
Implements (Finite) Categories by extending a Graph to support multiple
edges (seen as morphisms) between vertices (seen as a Category's Objects)
as well as some basic Category theory operations.
Another view of this is as ARGs - attributed relational graphs - where
each vertex and each edge is seen as having a set of attributes.
It can be made to extend Graph or RankedGraph. It also implements
a default IFilter that selects what information makes it into
an IGraph that visualises selected contentent from this Cat.
*/

public class Cat extends RankedGraph implements IFilter {
 
  /**
  Creates a new Category
   */
  public Cat() {
    super();
  }
  
  /**
   Creates a new Category
   */
  public Cat(int size) {
    super(size);
  }
  
  public RankedGraph new_instance() {
    return new Cat();
  }
  
  /* do not do this - it might ruin subclasses - the right thing
     not to inherit but explicitely delegate to Graph !!!
  */
     
  public void addVertex(Object vertex,Object data) {
    //JavaIO.errmes("error directly adding vertex in Cat:"+v+":"+data);
    setProp(vertex,"data",data);
  }
  
  public void addEdge(Object from,Object to,Object data) {
    //JavaIO.errmes("error directly adding edge in Cat:"+from+"=>"+to+":"+data);
    setMorphism(from,to,"data",data);
  }
  
  public Object vertexData(Object v) { 
    return getProp(v,"data");
  }

  protected Content toVertex(Object v) {
    return toVertex(v,new ObjectDict());
  }
  
  /**
   * Sets a the value of a property of this vertex.
   * setProp(Object vertex,Object key,Object value)
   */
  public Object setProp(Object vertex,Object key,Object value) {
    ObjectDict D=(ObjectDict)super.vertexData(vertex);
    if(null==D) {
      //Content C=
      toVertex(vertex);
      //D=(ObjectDict)C.data;
      D=(ObjectDict)super.vertexData(vertex);
    }
    if(null==value) {
      D.remove(key);
      return null;
    }
    return D.put(key,value);
  }
  
  /**
   * Gets a the vlaue of a property of this vertex.
   * Object getProp(Object vertex,Object key)
   */
  public Object getProp(Object vertex,Object key) {
    ObjectDict D=(ObjectDict)super.vertexData(vertex);
    if(null==D) return null;
    return D.get(key);
  }
  
  /**
   * Iterates over properties for give vertex.
   */
  public ObjectIterator propIterator(Object vertex) {
    ObjectDict D=(ObjectDict)super.vertexData(vertex);
    if(null==D) return null;
    return D.getKeys();
  }
   
  /**
   * Sets the value of a property of the link between two vertices - this can
   * be seen as defining a morphism between two Objects in a Category.
   * setMorphism(Object from,Object to,Object m,Object md)
   */
  public void setMorphism(Object from,Object to,Object m,Object md) {
    ObjectDict D=(ObjectDict)edgeData(from,to);
    if(null==D) {
      D=new ObjectDict();
      super.addEdge(from,to,D);
    }
    D.put(m,md);
  }
  
  /**
   * Gets the value of a property of the link between two objects - the can be seen
   * as selecting a morphism between two Objects in a Category.
   */
  public Object getMorphism(Object from,Object to,Object m) {
    ObjectDict D=(ObjectDict)edgeData(from,to);
    if(null==D) return null;
    return D.get(m);
  }
  
  /**
   * Iterates over morphisms between two objects.
   */
  public ObjectIterator morphismIterator(Object from,Object to) {
    ObjectDict D=(ObjectDict)edgeData(from,to);
    if(null==D) return null;
    return D.getKeys();
  }

  /**
   * Removes a morphism.
   */
  public Object removeMorphism(Object from,Object to,Object m) {
    ObjectDict D=(ObjectDict)edgeData(from,to);
    if(null!=D) {
      return D.remove(m);
    }
    return null;
  }
  
  /**
   *  A CatWalker visitor over elements of a Cat. 
   */
  public Object walk(CatWalker walker) {
    walker.atStart();

    walker.beforeProps();

    ObjectIterator Vs=vertexIterator();

    while(Vs.hasNext()) {
      Object V=Vs.next();
      ObjectIterator Ps=propIterator(V);
      while(Ps.hasNext()) {
        Object P=Ps.next();
        walker.onProp(V,P,getProp(V,P));
      }
    }
    
    walker.afterProps();

    walker.beforeMorphisms();

    Vs=vertexIterator();
    while(Vs.hasNext()) {
      Object V=Vs.next();
      ObjectIterator Ts=outIterator(V);
      while(Ts.hasNext()) {
        Object T=Ts.next();
        ObjectIterator Ms=morphismIterator(V,T);
        while(Ms.hasNext()) {
          Object M=Ms.next();
          walker.onMorphism(V,T,M,getMorphism(V,T,M));
        }
      }
    }

    walker.afterMorphisms();

    return walker.atEnd();
  }

  /**
   * Write out this Cat as XML.
   */
  public void to_xml(String fileName) throws Exception {
    walk(new CatXMLWriter(fileName));
  }

  /**
   * Gets a selected property of a vertex.
   */
  public Object getSelected(Object v) {
    if(ATTR==filter) return getProp(v,sfilter);
    else return super.getSelected(v);
  }
  
  /**
    adds random objects and morphisms (at most one between 2 objects)
  */
  
  /*
  public void randomize(int seed,int size,int degree) { 
    Random R;
    if(0==seed) R=new Random();
    else R=new Random(seed);
    for(int i=0;i<size;i++) {  
      String V=""+i;
      setProp(V,"V","V");
      int steps=1+R.nextInt(degree-1);
      for(int j=0;j<steps;j++) {
        int neighbor=R.nextInt(size);
        String N=""+neighbor;
        int luck=R.nextInt(4);
        if(0==luck) {
          setProp(N,"V","V");
          setMorphism(N,V,"E","E");
        }
        else if(1==luck) {
          setProp(N,"V","V");
          setMorphism(V,N,"E","E");
        }
        else {
          // do nothing 50% of the time
        }
      }
    }
  }
  */
 
  /*
   // Turns a Prolog Term into a Cat.
  
  static public Cat fromTerm(Object T) {
    Cat C=new Cat();
    ObjectDict shared=new ObjectDict();
    C.addTerm(T,shared,0);
    return C;
  }
  
   //deprecated - see termTree
   
  public Object addTerm(Object O,ObjectDict shared,int level) {
    Object S=shared.get(O);
    if(S!=null) return S;
    Object node=O;
    if(O instanceof Fun) {
      Fun T=(Fun)O;
      node=T.name+"_"+shared.size();
      shared.put(O,node);
      addVertex(node,O);
      this.setHyper(node,1);
      for(int i=0;i<T.args.length;i++) {
        String arg=node+":"+(i+1);
        addVertex(arg,new Integer(i+1));
        setHyper(arg,3);
          addEdge(node,arg,"level("+level+")");
        Object X=addTerm(T.args[i],shared,level+1); // recursion
          addEdge(arg,X,"argval");
      }
    }
    else {
      node=O.toString();
      addVertex(node,O);
      shared.put(O,node);
    }
    if(0==level) {
      String R="$root";
      shared.put(R,R);
      addVertex(R,R);
      setHyper(R,4);
      addEdge(R,node,R);
    }
    return node;
  }
  */
 

  static public Cat termTree(Object T) {
    return termTree(T,0,1);
  }

  /**
   * Turns a Prolog Term into a Tree-ish Cat.
   * Int params ftype and ctype can be used as hints by
   * visualizers for selecting the shapes/colors/details 
   * of function and constant vertices.
   */
  static public Cat termTree(Object T,int ftype,int ctype) {
    Cat C=new Cat();
    ObjectDict shared=new ObjectDict();
    C.addTermTree(new Term(T),shared,ftype,ctype,0);
    return C;
  }

  public Object addTermTree(Term O,ObjectDict shared,int ftype,int ctype,int level) {
    Object S=shared.get(O);
    if(S!=null) return S;
    Object node=O.getTerm();
    if(node instanceof Fun) {
      Fun T=(Fun)node;
      int d=shared.size();
      node=T.name+"_"+d;
      addVertex(node,T.name);
      shared.put(O,node);
      this.setHyper(node,ftype);
      for(int i=0;i<T.args.length;i++) {
        Object X=addTermTree(new Term(T.args[i]),shared,ftype,ctype,level+1); // recursion
        String arg_i=node+":"+(i+1);
        addVertex(arg_i,"");
        this.setHyper(arg_i,2);
        addEdge(node,arg_i,"");
        addEdge(arg_i,X,level+":"+i);
      }
    }
    else {
      node=O.toString();
      addVertex(node,O);
      shared.put(O,node);
      this.setHyper(node,ctype);
    }
    return node;
  }
 
  private IFilter iFilter=this;
  
  public void setIFilter(IFilter iFilter) {
    this.iFilter=iFilter;
  }
 
  public IFilter getIFilter() {
    return this.iFilter;
  }

  public Object filterVertex(Object rankedData) {
      RankedData RD=(RankedData)rankedData;
      if(null==RD.data) return "*";
      ObjectDict dict=(ObjectDict)RD.data;
      return dict.get("data");
    }
    public Object filterEdge(Object iEdgeData) {
      //return iEdgeData.getClass()+"@@"+iEdgeData.toString()+"@@";
      IEdge e=(IEdge)iEdgeData;
      if(null==e.data) return "-";
      //return e.data+":"+e.data.getClass();
      ObjectDict dict=(ObjectDict)e.data;
      return dict.get("data");
    }

  /*
  public static void ctest() {
    Cat C=new Cat();
    //C.addObject("a");C.addObject("b");
    C.setMorphism("a","b","ab1","1");
    C.setMorphism("a","b","ab2","2");
    C.setMorphism("b","c","bc1","3");
    C.setMorphism("b","c","bc2","4");
    C.setMorphism("c","b","cb1","5");
    C.setMorphism("c","a","ca1","6");
    C.setMorphism("e","e","ee1","7");
    C.setMorphism("f","g","fg1","8");
    C.setMorphism("f","g","fg2","9");

    C.setProp("b","age","42");
    C.setProp("b","weight",new Double(1/4));
    C.setProp("c","weight",new Double(3/4));
    C.runGraphRanker();
    C.rankSort();
    Prolog.dump("cat=\n"+C);
    C=(Cat)C.trimRankedGraph(2,20);
    Prolog.dump("trimmed=\n"+C);
    C=new Cat();
    C.randomize(0,2,20);
    C.trimRankedGraph(1,10);
    Prolog.dump("randomTrimmed=\n"+C);
  }
  */
}
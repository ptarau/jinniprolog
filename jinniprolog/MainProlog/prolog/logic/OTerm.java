package prolog.logic;

/**
 * allows building external terms represented as generic Object refrences in a uniform way
 * in particular, it allows interoperation with Kernel Prolog interpreter
 */
public interface OTerm {
  public Object putVar(int i) throws PrologException;
  public Object putConst(String c);
  public Object putInt(int i) ;
  public Object putFun(String f,Object[] args);
  public Object putFloat(double d);
  public int getTerm(Object t,ITerm I) throws PrologException;
}
package prolog.logic;

/**
 * allows building integer represented terms on a heap in a uniform way
 */
public interface ITerm {
  public int putVar(Object id) throws PrologException;
  public int putConst(String c) throws PrologException;
  public int putString(String s) throws PrologException;
  public int putInt(int i) throws PrologException;
  public int putFloat(double d) throws PrologException;
  public int putFun(String f,int[] args) throws PrologException;
  public int putObject(Object o) throws PrologException;
  public Object getTerm(int xref,OTerm O) throws PrologException;
}

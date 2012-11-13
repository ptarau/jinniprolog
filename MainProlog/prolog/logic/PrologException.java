package prolog.logic;

/**
 * Provides a generic Prolog Exception type
 */
public class PrologException extends Exception implements Stateful { // abstract
    /**
     * Constructs a <code>PrologException</code> with no detail message. 
     */
  public PrologException() {
	  super();
    show_error();
  }

    /**
     * Constructs an <code>PrologException</code> with the specified
     * detailed message. 
     *
     * @param   s   the message.
     */
  public PrologException(String s) {
	  super(s);
    show_error();
  }

  private void show_error() {
    if (Interact.quickfail>=2) Interact.printStackTrace(this);
  }

  public String toString() {
    String name=getClass().getName();
    int l=name.lastIndexOf(".");
    String Q="\'";
    if(l>0) name=name.substring(l+1,name.length());
    return "\'!! Prolog_Error\'(\'"+name+Q+","+Q+getMessage()+Q+"\')";
  }
}

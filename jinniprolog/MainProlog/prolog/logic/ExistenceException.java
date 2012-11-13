package prolog.logic;

/**
Exception indicating that something does not exist or has not been found.
 */
public class ExistenceException extends PrologException {
    /**
     * Constructs an <code>ExistenceException</code> with no detail message. 
     */
  public ExistenceException() {
	   super();
  }

  /**
     * Constructs an <code>ExistenceException</code> with the specified
     * detailed message. 
     *
     * @param   s   the message.
     */
  public ExistenceException(String s) {
	  super(s);
  }
  
}

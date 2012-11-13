package prolog.logic;

/**
Indicates that a resource has been exhausted - for instance memory.
 */
public class ResourceException extends PrologException {
    /**
     * Constructs an <code>ResourceException</code> with no detail message. 
     */
  public ResourceException() {
	   super();
  }

  /**
     * Constructs an <code>ResourceException</code> with the specified
     * detailed message. 
     *
     * @param   s   the message.
     */
  public ResourceException(String s) {
	  super(s);
  }
  
}

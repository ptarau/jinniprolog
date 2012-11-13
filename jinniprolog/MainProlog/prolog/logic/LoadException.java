package prolog.logic;

/**
Exception indicating that part of the Prolog runtime system
has failed to load - for reasons like file or URL stream corruption,
version mismatch or inconsistent data.
 */
public class LoadException extends PrologException {
    /**
     * Constructs an <code>LoadException</code> with no detail message. 
     */
public  LoadException() {
	   super();
  }

  /**
     * Constructs an <code>LoadException</code> with the specified
     * detailed message. 
     *
     * @param   s   the message.
     */
public  LoadException(String s) {
	  super(s);
  }
  
}

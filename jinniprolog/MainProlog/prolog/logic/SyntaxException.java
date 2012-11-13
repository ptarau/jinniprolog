package prolog.logic;

/**
Exception thrown on Systax Errors.
 */
public class SyntaxException extends PrologException {
  /**
   * Constructs an <code>SyntaxException</code> with no detail message. 
   */
  public SyntaxException() {
    super();
  }

  /**
     * Constructs an <code>SyntaxException</code> with the specified
     * detailed message. 
     *
     * @param   s   the message.
     */
  public SyntaxException(String s) {
    super(s);
  }

}

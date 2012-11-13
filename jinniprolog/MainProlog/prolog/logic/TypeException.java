package prolog.logic;

/**
Exception thrown when an unexpected dynamic type is detected.
 */
public class TypeException extends PrologException {
  /**
   * Constructs an <code>TypeException</code> with no detail message. 
   */
  public TypeException() {
    super();
  }

  /**
     * Constructs an <code>TypeException</code> with the specified
     * detailed message. 
     *
     * @param   s   the message.
     */
  public TypeException(String s) {
    super(s);
  }

}

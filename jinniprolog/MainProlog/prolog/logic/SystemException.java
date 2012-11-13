package prolog.logic;

/**
Exception indicating some internal data inconsistency.
 */
public class SystemException extends PrologException {
  /**
   * Constructs an <code>SystemException</code> with no detail message. 
   */
  public SystemException() {
    super();
  }

  /**
     * Constructs an <code>SystemException</code> with the specified
     * detailed message. 
     *
     * @param   s   the message.
     */
  public SystemException(String s) {
    super(s);
  }

}

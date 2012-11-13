package prolog.logic;

/**
 * Implements a wrapper to Prolog terms like Fun, Var
 * or Fun terms embedding various java Objects that can
 * be used to check for identity in dictionaries.
 * It ensures that Fun objects are tested for
 * deep equality and that their deep hashCode is used.
 */
public class Term implements Stateful {
  public Term(Object term) {
    this.term = term;
  }

  final private Object term;

  final public Object getTerm() {
    return this.term;
  }

  public boolean equals(Object O) {
    if(!(O instanceof Term)) return false;
    Term Other=(Term)O;
    Object other=Other.getTerm();
    if(term instanceof Fun) {
      if(!(other instanceof Fun)) return false;
      return ((Fun)(term)).deepEquals((Fun)other);
    }
    else
      return term.equals(other);
  }

  public int hashCode() {
    if(term instanceof Fun) return ((Fun)term).deepHashCode();
    else 
      return term.hashCode();
  }

  public String toString() {
    return term.toString();
  }
}

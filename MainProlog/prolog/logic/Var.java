package prolog.logic;

/**
 * Implements a lightweight external representation of Prolog
 * variables - each Var wraps an int id which makes it unique.
 */
public class Var implements Stateful {
  public Var(int id) {
    this.id = id;
  }

  final private int id;

  final public int getID() {
    return id;
  }

  public boolean equals(Object O) {
    return O instanceof Var && ((Var)O).getID() == id;
  }

  public int hashCode() {
    return id;
  }

  public String toString() {
    return "_j" + id;
  }
}

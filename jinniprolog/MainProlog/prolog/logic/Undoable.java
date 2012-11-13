package prolog.logic;

/**
Java data subject to undo action on backtracking should implement this.
 */
public interface Undoable {
  public void undo();
  public void done();
}
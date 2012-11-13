package prolog.logic;

/**
Instructs the current machine to apply undo action to a key.
 */
public class UndoKey implements Undoable, Stateful {
  private Object key;
  private LogicEngine machine;

  public UndoKey(Object key, LogicEngine machine) {
    this.key = key;
    this.machine = machine;
  }

  public void undo() {
    // Prolog.dump("undo: remove key="+key);
    this.machine.uremove(key);
  }

  public void done() {
    // nothing left to do
  }


}
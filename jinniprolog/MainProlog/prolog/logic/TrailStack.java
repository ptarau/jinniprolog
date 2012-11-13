package prolog.logic;
/**
* Implementation of the Trail Stack for the Prolog machine.
* As variables are bound during execution the Trail records the heap index of those variables.
* When backtracking those indexes are used to reset the associated variables back to their unbound
* state.
*/
final class TrailStack extends IntStack {

  /** Access to the Choice Point Stack for the current Machine. */
  private ChoicePointStack choice;

  /** Access to the Heap for the current Prolog */
  private HeapStack heap;

  /**
  * Create a Trail Stack.
  * @param machine The Machine instance with which this trail stack is associated.
  * @param trailMax The required size of the trail stack.
  * @param heap The HeapStack instance associated with machine.
  * @param choice The ChoicePointStack instance associated with machine.
  */
  TrailStack(HeapStack heap, int trailMax, ChoicePointStack choice) {
    super(trailMax);
    this.heap = heap;
    this.choice = choice;
  }

  /** Trail heap index of a variable which has just been bound.
  * @param hRef The heap index of the variable.
  */
  final void trailVar(int hRef) {
    //trail[++trailTop] = hRef;	
    push(hRef);
  }

  /** Conditionally trail heap index of a variable which has just been bound.
  *<p>If the variable has been created since the last choice point it will become
  * unreachable on backtracking. There is no need to trail such variables.
  * @param hRef The heap index of the variable.
  */
  final void trailVarIf(int hRef) {
    if (hRef <= choice.SAVED_H())
      //trail[++trailTop] = hRef;	
      push(hRef);
  }

  /** Unbind variables bound since a specified offset into the trail stack.
  * @param savedTop The trail stack index after which new bindings were added.
  */
  void unwindTrail(int savedTop) {
    while (savedTop < getTop()) {
      int href = pop();
      heap.setRef(href, href);
    }
  }

  protected void expand() {
    heap.startMMtimer();
    super.expand();
    heap.endMMtimer();
  }

  void tidy() {
    if (null != choice) compact(choice.SAVED_TR(), choice.SAVED_H());
  }

  void dump() {
    Prolog.dump(Interact.NL + "TRAIL:" + Interact.NL + this);
  }

} // End class TrailStack

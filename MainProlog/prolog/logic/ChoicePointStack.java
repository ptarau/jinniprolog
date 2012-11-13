package prolog.logic;

/**
 *  Implements the BinWAM engine's choice point stack
 */
final class ChoicePointStack extends ObjectStack {

private HeapStack heap;
private TrailStack trail;

ChoicePointStack(HeapStack heap, int choiceMax) {
  super(choiceMax);
	this.heap = heap;
}

final void setTrail(TrailStack trail) { 	this.trail = trail; }

final int SAVED_H() {return ((ChoicePointStackEntry)peek()).heapTop; }
final int SAVED_TR() {return ((ChoicePointStackEntry)peek()).trailTop; }
final int SAVED_P() {return ((ChoicePointStackEntry)peek()).instrPtr; }
final int setSAVED_P(int instrPtr) {return ((ChoicePointStackEntry)peek()).instrPtr = instrPtr; }

/**
Adds a choicepoint to the stack.
*/
final int addChoicePoint(int instrPtr, int[] regs, int regCount)   {
  push(new ChoicePointStackEntry(heap.getHeapTop(),trail.getTop(),instrPtr,regs,regCount));
	return getTop();
}

protected void expand() {
  heap.startMMtimer();
  super.expand();
  heap.endMMtimer();
}
/**
 * Aggressive space recovery mechanism - tries to
 * shrink all eligible data areas. By calling
 * this on backtrack, cut, and heap overflow one can
 * make sure that no Prolog engine locks memory
 * resources inadvertantly.
 */
void shrinkAll() {
   heap.startMMtimer();
   trail.tidy();
   // collectTrail();
   trail.shrink();
   this.shrink();
   heap.shrink();
   heap.endMMtimer();
}
 
final void setCut(int cutBint) {
	setTop(cutBint);
  shrinkAll(); // shrinks on forward execution - see also gc
}

final int restoreState(int[] regs, boolean discardChoice) {
	trail.unwindTrail(SAVED_TR());
	heap.setHeapTop(SAVED_H());
	int[] savedRegs = ((ChoicePointStackEntry)peek()).regs;
	System.arraycopy(savedRegs,0,regs,1,savedRegs.length);
  
  ((ChoicePointStackEntry)peek()).undo();
  
  if (discardChoice) {
    
    ((ChoicePointStackEntry)peek()).done();
    
    pop();
    shrinkAll(); // aggressive space recovery 
		return getTop();
  }
  
	return getTop() - 1;
}

void destroy() {
  while(!isEmpty()) {
    ChoicePointStackEntry cp=(ChoicePointStackEntry)pop();
    cp.undo();
    cp.done();
  }
  super.destroy();
}
/*
  // trail trimmer
   
  final void collectTrail() {
    //PrologGC.dumpTrail(trail,heap,trail.getTop());
    
    int min=0;int max=-1;
    for(int i=0; i<this.size(); i++) {
      ChoicePointStackEntry cp=((ChoicePointStackEntry)this.at(i));  
      max=collectTrail(min,cp.trailTop);
      cp.trailTop=max;
      min=max+1;
    }
    if(max>=0)
      trail.setTop(max);
    PrologGC.dumpTrail(trail,heap,trail.getTop());
   
  }
   
  final int collectTrail(int from, int max) {
    while (from <= max) {
      int h=trail.at(from);
      if (0!=h) {
		    from++; //keep
		  }    
      else {
		    trail.update(from,trail.at(max)); //remove
        trail.update(max,0);
        max--;
      }
	  }
    return max;
	}
*/

} // End class ChoicePointStack

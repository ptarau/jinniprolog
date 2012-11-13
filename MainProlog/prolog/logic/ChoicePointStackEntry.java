package prolog.logic;

/**
 * ChoicePoint entry containing heapTop,trailTop,instrPtr fields and register array regs
 */
final class ChoicePointStackEntry implements Stateful {
  
   ChoicePointStackEntry(int heapTop,int trailTop,int instrPtr,int[] regs,int regCount) {
     this.heapTop=heapTop;
     this.trailTop=trailTop;
     this.instrPtr=instrPtr;
     this.regs= new int[regCount];
     System.arraycopy(regs,1,this.regs,0,regCount);
     undoStack=null;
  }
  
	 int heapTop;
   int trailTop;
   int instrPtr;
   int[] regs;
   ObjectStack undoStack;
   
   final void add(Undoable O) {
     if(null==undoStack) undoStack=new ObjectStack();
     undoStack.push(O);
   }
   
   public void undo() {
     if(null==undoStack) return;
     
     for(int i=undoStack.getTop();i>=0;i--) {
       Undoable O=(Undoable)undoStack.at(i);
       O.undo();
     }
   }
   
   final void done() {
     if(null==undoStack) return;
     
     for(int i=undoStack.getTop();i>=0;i--) {
       Undoable O=(Undoable)undoStack.at(i);
       O.done();
     }
     undoStack=null;
   }
   
   public String toString() {
      return "(H="+heapTop+",TR="+trailTop + ",P="+instrPtr+",regs="+regs.length+")";
   }
} // End class ChoicePointStackEntry
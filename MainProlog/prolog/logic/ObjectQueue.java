package prolog.logic;

/**
  Generic dynamic Queue with (amortized) O(1)
  enq/deq (add and remove) operations.
*/
public class ObjectQueue implements Stateful { 
  final static int MINSIZE=4;
  final static int MAX_QUEUE=1<<24;

  private int head,tail;
  private Object[] queue;
  private int current;
  
  
  public ObjectQueue(int size) { 
    makeIt(size);
  }
         
  public ObjectQueue() {
    this(0);
  }
 
  public ObjectQueue(Object[] os) {
    this(os.length);
    for(int i=0; i<os.length; i++)
      enq(os[i]);
  }        
 
  synchronized public final void clear() {
    makeIt(0);
  }
          
  synchronized private final void makeIt(int size) {
    size=(size<MINSIZE)?MINSIZE:size;
    queue = new Object[size];
    head = tail = current = 0;
  }

  public final int size() {
    return (head<=tail)?tail-head:queue.length-head+tail;
  }

  /**
   Dynamically resizes the queue
   */
  private final boolean requeue(String Mes) {
    int newSize=2*size();
    if(newSize>MAX_QUEUE || newSize<MINSIZE) return false;
    Object[] nqueue=new Object[newSize];
    int j=0;
    for(int i=head; i!=tail; i=inc(i))
      nqueue[j++]=queue[i];
    queue=nqueue;
    head=0;
    tail=j;
    //Prolog.dump("resized_to:"+newSize);
    return true;
  }


  /**
   Adds an element to the end of the queue
   */
  synchronized public final boolean enq(Object V) { 
      
    if(inc(tail) == head) { // full !!!
      if(!requeue("expanding")) {
        Interact.warnmes("queue overflow at:"+V);
        return false;
      }
    }     
    queue[tail] = V;
    tail=inc(tail);
             
    return true;
  }
          
          
  /**
   Removes the first element of the queue 
   */
  synchronized public final Object deq() {    
             
    if (tail == head) // empty !!!
      return null;
    if(4*size()<queue.length)
      requeue("shrinking");
    Object V = queue[head];
    queue[head]=null;
    head  = inc(head);
             
    return V;
  }

  /**
   Adds an element to the beginning of the queue
   */
  synchronized public final boolean pushq(Object V) { 
           
    if(dec(head) == tail) { // full !!!
      if(!requeue("expanding")) {
        Interact.warnmes("queue overflow at:"+V);
        return false;
      }
    }  
    head=dec(head);
    queue[head] = V;
             
    return true;
  }
          
  private final int inc(int val) {         
    return (val + 1) % queue.length; 
  }

  private final int dec(int val) {
    return (0==val)? queue.length-1:val-1;
  }
          
  synchronized public final boolean isEmpty() {
    return tail == head;
  }
          
  synchronized public final Object[] toArray() {
    Object[] Os=new Object[size()];
    int j=0;
    for(int i=head; i!=tail; i=inc(i))
      Os[j++]=queue[i];
    return Os;
  }      
 
  public ObjectQueue toClone() {
    Object[] os=toArray();
    return new ObjectQueue(os);
  }     
  
  synchronized public final Object elementAt(int i) {
    return queue[(head+i) % queue.length];
  }
  
  synchronized public final void updateAt(int i,Object O) {
    queue[(head+i) % queue.length]=O;
  }
          
  synchronized public final int contains(Object O) {
    for(int i=head; i!=tail; i=inc(i)) {
      if(O.equals(queue[i])) return i;
    }
    return -1;
  }
          
  synchronized public final void delq(Object O) {
    int k=contains(O);
    if(k<0) return;
    deleteAt(k);
  }
  
  synchronized public final void deleteAt(int k) {
    /* buggy
    for(int i=head; i!=k; i=inc(i)) {
      queue[i]=queue[inc(i)];
    }
    deq();
    */
    
    ObjectQueue Q=new ObjectQueue();
    int i=0;
    while(!isEmpty()) {
      Object O=deq();
      if(i++!=k) {
        Q.enq(O);
      }
    }
    head=Q.head;
    tail=Q.tail;
    queue=Q.queue;
  }
  
  // queue_iterator ops
  
  synchronized public void reset() {
    current=head;
  }
  
  synchronized public Object next() {
    if(!hasNext()) return null;
    Object O=queue[current];
    current=inc(current);
    return O;
  }
  
  synchronized public boolean hasNext() {
    return !isEmpty() && inc(current)!=head;
  }
  
  synchronized public void del() { // TODO:  $BUGGY
    //System.err.println("current_bef="+current+" ,head="+head+", tail="+tail);

    deleteAt(dec(current));
    reset(); // current does not make sense anymore !!!
    
    //updateAt(dec(current),null);
    //System.err.println("current_aft+="+current+" ,head="+head+", tail="+tail);
    //System.err.println("queue="+toString()); 
  }
  
  public Object queue_op4(int op,int i) {
    Object R=null;
    switch(op) {
      case 0: if(i>=0 && i<size()) R=elementAt(i);break;
      case 1: reset(); break;
      case 2: R=next();break;
      case 3: del(); break; // buggy
      default: Prolog.dump("warning - bad queue operation: op="+op+",i="+i);
    }
    return R;
  }
          
  public String toString() {
    //return "queue:"+size()+"/"+queue.length;
    StringBuffer buf=new StringBuffer();
    for(int i=head; i!=tail; i=inc(i)) {
      if(i!=head) buf.append(",");
      buf.append(elementAt(i));
    }
    return buf.toString();
  }

  /*
   static public void test(int N){
   ObjectQueue O=new ObjectQueue();Prolog.dump("queue:"+O);
   O.enq("10");Prolog.dump("queue:"+O);
   O.pushq("1");Prolog.dump("queue:"+O);
   O.enq("20");Prolog.dump("queue:"+O);
   O.pushq("2");Prolog.dump("queue:"+O);
   O.enq("30");Prolog.dump("queue:"+O);
   O.pushq("3");Prolog.dump("queue:"+O);
   O.enq("40");Prolog.dump("queue:"+O);
           
   Prolog.dump("deq:"+O.deq());Prolog.dump("queue:"+O);
   Prolog.dump("deq:"+O.deq());Prolog.dump("queue:"+O);
   Prolog.dump("queue:"+O);Prolog.dump("queue:"+O);
   Prolog.dump("deq:"+O.deq());Prolog.dump("queue:"+O);
   Prolog.dump("deq:"+O.deq());Prolog.dump("queue:"+O);
   Prolog.dump("deq:"+O.deq());Prolog.dump("queue:"+O);
   Prolog.dump("deq:"+O.deq());Prolog.dump("queue:"+O);
   Prolog.dump("deq:"+O.deq());Prolog.dump("queue:"+O);
   Prolog.dump("deq:"+O.deq());Prolog.dump("queue:"+O);
   for(int i=0;i<N;i++) {
   O.enq(new Fun("a",new Integer(i)));
   }
   for(int i=0;i<N;i++) {
   O.pushq(new Fun("a",new Integer(i)));
   }
   for(int i=0;i<2*N;i++) {
   O.deq();
   }
   Prolog.dump("size="+O.size());
            
   }
  */ 
}


 
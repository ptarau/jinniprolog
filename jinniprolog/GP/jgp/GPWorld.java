package jgp;

import prolog.logic.*;
import prolog.kernel.*;
import prolog.core.*;
import rli.RLIAdaptor;
import java.math.*;
import java.util.Random;

/**
 * Top GP class: creates a world in which evolving individuals
 * seek a minimal expression tree matching a model specified as
 * a truth table, in the form of a BigInteger. The world can be
 * customized to support various distance implementations and 
 * evaluators.
 */
public class GPWorld extends BigMath implements Stateful,Runnable {
 
  /**
   * Static constructor call with exception catching.
   */
  public static GPWorld makeWorld() {
    try {
      //return new GPWorld("0110100110010110",new EvalITE(4));
      //return new GPWorld("01101001",new EvalITE(3));
      return new GPWorld("0110",new EvalITE(2));
      //return new GPWorld(new EvalITE(3));
    }
    catch(Exception e) {
      e.printStackTrace();
      return null;
    }
  }

  /**
   * Creates an evolvable world using a given
   * formula evaluator.
   * 
   */
  public GPWorld(Eval evaluator){
    this(ranbig(evaluator.getNvars()),evaluator,new HammingDist());
  }
  
  public GPWorld(String smodel,Eval evaluator){
    this(new BigInteger(smodel,2),evaluator,new HammingDist());
  }
  
  public GPWorld(BigInteger model,Eval evaluator){
    this(model,evaluator,new HammingDist());
  }
  
  /**
   * Creates a new GPWorld. If needed, parmeters can be set after this but
   * should be set before calling run().
   */
  public GPWorld(BigInteger model,Eval evaluator,DistEval distEval){
    this.model=model;
    this.evaluator=evaluator;
    this.distEval=distEval;
    this.nvars=evaluator.getNvars();
    this.npower=1<<nvars;
    this.gbits=1<<(nvars+2);
    this.maxsteps=Math.max(1<<20,(gbits<64)?1<<(nvars+nvars):Long.MAX_VALUE);
    this.maxpop=nvars*gbits;
    this.maxperf=npower;
    Inds=new ObjectQueue();
    Perfs=new ObjectQueue();
  }

  public static BigInteger ranbig(int nvars) {
    return new BigInteger(1<<nvars,rand);
  }  

  // public static Random rand = prolog.logic.Tools.getRandom();
  public static Random rand=new Random();

  public static int vtrace=2;

  /**
   * number of "ur-elements" interpreted as independent variables
   */
  public final int nvars;

  /**
   * size of the phenotype seen as a truth table
   */
  public final int npower;

  /**
   * size of the genotype seen as an encoding of an expression DAG, in bits
   */
  public final int gbits;

  /**
   * max number of the total events happening to various individuals
   */
  public final long maxsteps;

  /**
   * max number of individuals alive at a given time
   */
  public final int maxpop;

  /**
   * max number of "perfect" individuals before the search concludes
   */
  public final int maxperf;
  
  /**
   * model towards which the population tries to converge, seen as the truth
   * table specifying a function
   */
  final protected BigInteger model;

  final protected Eval evaluator;
  
  final private DistEval distEval;
  /**
   * parameters expressed as percentages controlling the chance for event types
   * to happen to a given individual
   */

  public int MUTATE=5; // % is the dif with 0

  public int NEGATE=10; // % is the dif with previous

  public int GROW=20; // % is the dif with previous
  
  public int IMITATE=90; // % is the dif with previous

  public int CREATE=100; // % is the dif with previous

  private long steps=0;

  /**
   * starts a GPWorld as a Thread
   */
  public static void run_bg() {
    Runnable R=makeWorld();
    if(null==R) return;
    Thread T=new Thread(R,"GPThread");
    T.start();
  }

  /**
   * starts a GPWorld as a Thread
   */
  public static void run_fg() {
    Runnable R=makeWorld();
    if(null!=R) R.run();
  }

   /**
   * Multiset of Individuals at a given time, organized as a Queue
   */
  public ObjectQueue Inds;

  /**
   * set of individuals that are "perfect" in the sense of
   * meeting the spec defined by their model
   * 
   * the remaining optimization element for the "perfects" is reduction
   * of their bitstring size i.e. having minimal BigInteger phenotypes
   * stored in the instance variable "body"
   */
  public ObjectQueue Perfs; 
  
  
    
    /**
   * returns a distance measuring ho close this Ind is to the specification
   * induced from a given model.
   */
    public int distanceToModel(Ind ind) {
      return this.distEval.distance(evalPheno(ind),model);
    }
    
    /**
     * Computes by interpreting the "genotype" a "phenotype" subject to be
     * tested for "closeness" to the "model" this individual seeks.
     */
    public BigInteger evalPheno(Ind ind) {
      return evaluator.eval(ind.getGeno());
    }
    
  /**
   * creates an individual
   * 
   * @return
   */
  public Ind create(boolean verbose) {
    BigInteger body=new BigInteger(gbits, GPWorld.rand);
    BigInteger seen=BigInteger.valueOf(steps);
    if(body.compareTo(seen)<=0) body=one.shiftLeft(gbits).subtract(seen);
    body=body.add(seen);
    body=body.and(BigMath.bigones(gbits));
    //Ind I=new Ind(body,this.model,nvars,gbits);
    Ind I=new Ind(body,this);
    if(verbose && times()) I.show("CREATE");
    Inds.enq(I);
    return I;
  }

  public Ind tryNext() {
    return new Ind(BigInteger.valueOf(steps),this);
    // Inds.enq(I);
  }

  public Ind negate() {
    int i=rand.nextInt(Inds.size());
    Ind ind=(Ind)Inds.elementAt(i);
    return ind.negate(times());
  }
  
  public Ind grow() {
    int i=rand.nextInt(Inds.size());
    Ind ind=(Ind)Inds.elementAt(i);
    return ind.grow(times());
  }
  
  /**
   * Mutate and individual.
   * 
   */
  public Ind mutate() {
    int i=rand.nextInt(Inds.size());
    Ind ind=(Ind)Inds.elementAt(i);
    return ind.mutate(times());
  }

  /**
   * Picks an individual that imitates another, provided the other is closer to
   * the model. Imitation can be implemeted by flipping a bit that was different
   * from the closer other individual.
   */
  public Ind imitate() {
    boolean show=times();
    int j=2+rand.nextInt(Inds.size()-2);
    int i=rand.nextInt(j);
    Ind I=(Ind)Inds.elementAt(i);
    Ind J=(Ind)Inds.elementAt(j);
    //if(I.equals(J)) return null;
    
    int betterIJ=I.compareTo(J);
    // I.show("I");
    // J.show("J");
   
    if(betterIJ<0) {
      return J.imitate(I,show);
    } else if(betterIJ>0) {
      return I.imitate(J,show);
    } else {
      //System.out.println(I + "=" + J);
      //return J.imitate(J,show);
      return J.mutate(show);
    }
    
    //return J.imitate(I,show);
  }

  public boolean perfectNext() {
    Ind next=tryNext();

    if(next.isPerfect()) {
      Perf perf=new Perf(next,this);
      Perfs.enq(perf);
      perf.show("PERFECT NEXT");
      return true; // we know this is minimal
    }
    
    next=next.grow(times());
    
    if(next.isPerfect()) {
      Perf perf=new Perf(next,this);
      Perfs.enq(perf);
      perf.show("PERFECT NEXT AFTER GROWTH");
      return true; // we know this is minimal
    }
    
    return false;

  }

  public boolean seed() {
    int popsize=(int)maxpop/2;
    for(int i=0;i<popsize;i++) {
      if(perfectNext())
        return true;
      Ind perfect=null;
      perfect=create(false);
      if(perfect.isPerfect()) {
        checkPerfect(perfect);
      }
    }
    steps+=popsize;
    return false;
  }

  /**
   * Implements an evolution cycle, starting by creating a few root individuals
   * and then applying randomly evolution steps to random individuals.
   */
  public void evolve() {
    steps=0;
    Ind perfect=null;

    if(seed()) return;

    for(;steps<maxsteps;steps++) {

      if(perfectNext()) return;

      int dice=rand.nextInt(CREATE);
      // System.out.println("dice=" + dice);
      if(dice<MUTATE) {
        perfect=mutate();
      } 
      if(dice<NEGATE) {
        perfect=negate();
      } 
      if(dice<GROW) {
        perfect=grow();
      } 
      else if(dice<IMITATE) {
        perfect=imitate();
      } 
      else {
        // System.out.println("create="+dice);
        perfect=create(true);
      }
      
      //System.out.println("dice="+dice);
      
      if(null!=perfect && perfect.isPerfect()) {
        if(checkPerfect(perfect)) {
          if(Perfs.size()>maxperf) return;
        };
        perfect=null;
      }

      controlPopulation();

      showProgress();
    }

  }

  public void controlPopulation() {
    int l=Inds.size();
    if(l<maxpop)
      return;
    //System.out.println("\nInds before="+Inds+queueToString(Inds));
    cleanSort(maxpop/2);
    //System.out.println("\nInds after="+Inds+queueToString(Inds));
  }

  /**
   * Runs an evolution cycle for a population and shows the results - including
   * a possible "perfect" individual matching the model seeked by the
   * evolutionary process.
   */
  public void run() {
    try {
      showParams();
      evolve();
      showBest();
    }
    catch(Exception e) {
      e.printStackTrace();
    }
  }

  public boolean checkPerfect(Ind perfect) {
    if(Perfs.contains(perfect)<0) {
      if(Perfs.size()<5)
        showPerfect(perfect);
      Perf clone=new Perf(perfect,this);
      Perfs.enq(clone);
      perfect.mutate(times());
      return true;
    }
    return false;
  }

  public static ObjectQueue sort(ObjectQueue Os) {
    Object[] os=Os.toArray();
    Tools.sort(os);
    Os=new ObjectQueue(os);
    return Os;
  }

  /**
   * Sorts the population and eliminates dead individuals.
   */
  public void cleanSort(int max) {
    Object[] inds=this.Inds.toArray();
    Tools.sort(inds);
    // System.out.println("INDS_LEN="+inds.length);
    this.Inds=new ObjectQueue();
    for(int i=0;i<inds.length&&max>0;i++) {
      Ind I=(Ind)inds[i];
      if(i>0&&I.equals((Ind)inds[i-1]))
        continue;
      this.Inds.enq(inds[i]);
      max--;
    }
  }

  /**
   * returns a String representation of this world as a list of representations
   * of its individuals alive.
   */
  public String queueToString(ObjectQueue Os) {
    StringBuffer buf=new StringBuffer("[");
    for(int i=0;i<Os.size();i++) {
      if(i>0)
        buf.append(',');
      buf.append(Os.elementAt(i));
    }
    buf.append("]");
    return buf.toString();
  }

  public void showPerfect(Ind perfect) {
    System.out.println("\n! FOUND PERFECT at step="+steps+"="+perfect);
    perfect.show("PERFECT");
    if(vtrace>1)
      rshow(perfect);
  }
  
  /**
   * shows this GPWorld
   */
  public Ind pickBest() {
    if(Perfs.size()>0) {
      Perfs=sort(Perfs);
      Perf best=(Perf)Perfs.deq();
      Perfs.pushq(best);
      return best;
    } else {
      cleanSort(2);
      Ind best=(Ind)Inds.deq();
      return best;
    }
  }

  public void showParams() {
    System.out.println("PARAMS:"+"nvars="+nvars+",gbits="+gbits+",maxpop="
        +maxpop+",maxsteps="+maxsteps+"\n");
    System.out.println("VARS:\n"+evaluator.showVars());
  }

  public void showBest() {
    // System.out.println("PERFS:"+queueToString(Perfs));
    System.out.println("PERFS:"+Perfs.size()+",INDS:"+Inds.size());

    Ind best=pickBest();
    String s="APPROX. BEST";
    if(best instanceof Perf)
      s="PERFECT BEST";
    String M=Ind.big2string(model,npower);
    System.out.println("\n"+s+":"+best+"=?="+M+":(model)"+",VAL="+best.getGeno());
    if(vtrace>0)
      rshow(best);
  }

  boolean times() {
    long each=1000*(1<<Math.max(3,nvars));
    return steps%each==0;
  }

  public void showProgress() {
    if(times())
      System.out.println("Steps:"+steps+",Inds:"+Inds.size()+",Perfs:"
          +Perfs.size());
  }

  public void rshow(Ind ind) {
    int ccount=2;
    System.out.println("!!!:"+ind+"=>\n"+evaluator.toExpr(ind.getGeno()));
    rshow(nvars+ccount,evaluator.getArity(),ind.getGeno());
  }
  
  public static void rshow(int maxur,int arity,BigInteger B) {
    Cat C=bigint2cat(maxur,arity,B);
    Fun G=new Fun("rshow",C);
    RLIAdaptor.rli_call("localhost","renoir",G);
    try {
      Thread.sleep(1000);
    } catch(Exception e) {
    }
  }
}

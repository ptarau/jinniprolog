package prolog.logic;

/**
 *  Shows examples of calls form Java to Prolog.
 *  Test with lcall0 from full Prolog.
 */
public class Start {

  /**
   *  this class embeds a LogicEngine into your application
   */
  public static void main(String args[]) {
    String start=null;
    if(args!=null && args.length>0)  Interact.PROLOG_BYTECODE_FILE=args[0];
    if(args!=null && args.length>1)  start=args[1];
    if (null!=start) { // do a goal give as arg 1 input 1 output arg
       Object goal=new Fun(":-",new Var(1),new Fun(start,new Var(1)));
       Interact.println("Query of file="+args[0]+"=>"+goal);
       Interact.println("Result=> " + LogicEngine.call(goal));
       Interact.halt(0);
    }
    // various tests otherwise
    Interact.println("Logic Engine Test with bytecode file: " + Interact.PROLOG_BYTECODE_FILE);
    Object goal;
    goal=new Fun(":-",new Var(1),new Fun("eq",new Var(1),"hello"));
    Interact.println("Result=> " + LogicEngine.call(goal));
    goal=new Fun("*",new Integer(2),new Double(3.14),new Var(1));
    Interact.println("Result=> " + LogicEngine.call(goal));
    goal="fail";
    Interact.println("Result=> "+LogicEngine.call(goal));
    goal = new Fun("puretest",new Var(1));
    Interact.quickfail = 3;
    Interact.println("Result=> " + LogicEngine.call(goal));
    Interact.halt(0);
  }
}
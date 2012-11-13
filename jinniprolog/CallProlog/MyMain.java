import prolog.kernel.*;
import prolog.logic.*;
import java.util.*;
import java.io.*;
/**
 *  Shows examples of calls form Java to Prolog and back
 */
public class MyMain  { 
 
  /**
   *  this class embeds Jinni into your application
   *  make sure prolog.jar is copied in THIS directory
   *  because that's where Jinni will look for its Java code
   *  as well as the for its Prolog bytecode.
   *  Start Jinni with: go.bat, then run.bat
   */
  public static void main (String args[]) { 
    JavaIO.println("basic engine test=>"+ 
      LogicEngine.call(new Fun("eq",new Var(1),"hello"))
    );
    Machine M=Top.initProlog(args); 
    if(M!=null) {
      String query="compile('myMain')";
        testJavaPrologInterface();
        M.run(query);
      //Top.toplevel();
    }
    JavaIO.halt(0);
  }

  /**
   * just makes printing easier
   */
  public static void p(String s) {
    JavaIO.dump(s);
  }

/**
 *  creates a Prolog machine and runs on in a few queries
 */  
public static void testJavaPrologInterface() {

  Machine M=Top.new_machine();
  
  {
  // simple String queries
  
    String s=M.run("member(X,[a,b,c])");
    JavaIO.dump("testJinni: first X in member/3: "+s);   
     
    M.run("assert(a(88))");
    JavaIO.dump("assert works: " + M.run("X:-a(X)"));    
      
    // building a compound term query, getting all answers
 
    Object X=new Var(1);
    Object goal_args[]={X,new Integer(1),new Integer(5)};
    Fun Goal=new Fun("for",goal_args);
    Object[] answer_args={X,Goal};
    Fun Query=new Fun(":-",answer_args);
    if(!M.load_engine(Query)) return;
    for(;;) {
      Object answer=M.get_answer();
      if(null==answer) {
        // this kills the machine - if still alive - 
        // it happens after last answer automatically
        M.stop(); 
        break;
      }
      JavaIO.dump("testJinni: X in for/3(X,1,5): "+answer);
    }
  }  

  { // passing to Prolog a Java object for a call back method
  
    M=Top.new_machine(); // stop killed the previous one
    Date today=new Date();
    Object R=new Var(1);
    Object goal_args[]={today,"toString",R}; // note first arg "today" - it is a handle to a Date
    Fun Goal=new Fun("invoke_java_method",goal_args); // we build the callback goal
    Object[] answer_args={R,Goal}; // we build the answer pattern to be returned
    Fun Query=new Fun(":-",answer_args); // we put them together as a query with clause syntax
    if(!M.load_engine(Query)) return; // we load the (existing) Prolog engine
    for(;;) {
      Object answer=M.get_answer(); // get an answer
      if(null==answer) {
        M.stop();
        break; // exit loop when finished
      }
      // print out an answer
      JavaIO.dump("testJinni: Prolog callback on a Java Date object gives ===> "+answer);
    }
  }  
  
  {
    // simple String query with output written by Prolog collected into a String
    ByteArrayOutputStream output=new ByteArrayOutputStream();
    M=Top.new_machine(null,new PrologWriter(output));
    String query="X:-member(X,[a,b,c]),println(writing(X)),fail;X=done";
    String answer=M.run(query);
    JavaIO.dump("query with output redirected to String:\n"+query);
    JavaIO.dump("collected output=>\n"+output.toString());
    JavaIO.dump("computed answer =>"+answer);
  }
  
  {
    // simple String query with input and output from/to String
    String goal="for(I,1,3),println(I),fail;true. ";
    ByteArrayInputStream input=new ByteArrayInputStream(goal.getBytes());
    ByteArrayOutputStream output=new ByteArrayOutputStream();
    M=Top.new_machine(new PrologReader(input),new PrologWriter(output));
    String query="Answer:-read(Goal),println(read_from_string=Goal),call(Goal),fail;Answer=done";
    String answer=M.run(query);
    JavaIO.dump("query with I/O from/to a String:\n"+query);
    JavaIO.dump("goal read from a String:\n"+goal);
    JavaIO.dump("collected output=>\n"+output.toString());
    JavaIO.dump("computed answer =>"+answer);
  }


  {
    // general query answer with input/output args
  
    String toRead="end_of_file";
    ByteArrayInputStream input=new ByteArrayInputStream(toRead.getBytes());
    ByteArrayOutputStream output=new ByteArrayOutputStream();
    M=Top.new_machine(new PrologReader(input),new PrologWriter(output));
    Object[] args=new Object[2];args[0]="first";args[1]="second";
    
    for(int i=0;i<args.length;i++) {
      Fun aQuery=new Fun("assert",new Fun("get_i",new Integer(i),args[i]));
      if(!M.load_engine(aQuery)) return;
      Object answer=M.get_answer();
    }
    Var I=new Var(1);
    Var X=new Var(2);
    Fun bQuery=new Fun("foreach",new Fun("get_i",I,X),new Fun("assert",new Fun("put_i",I,X)));
    if(!M.load_engine(bQuery)) return;
      Object answer=M.get_answer();
      JavaIO.dump("!!!"+answer);

    Fun cQuery=new Fun(":-",X,new Fun("put_i",I,X));
    if(!M.load_engine(cQuery)) return;
    for(;;) {
      answer=M.get_answer(); // get an answer
      if(null==answer) {
        M.stop();
        break; // exit loop when finished
      }
      JavaIO.dump("!!!"+answer);
    }
    M.stop();
  }

  { // some easier ways to build queries - using special purpose Fun constructors
    
    M=Top.new_machine(); // stop killed the previous one
    Fun List=new Fun(".","a",new Fun(".","b","[]"));
    Fun Goal=new Fun("append",new Var(1),new Var(2),List); // we build the Prolog goal
    Fun Query=new Fun(":-",new Fun("result",new Var(1),new Var(2)),Goal); // we put them together as a query with clause syntax
    if(!M.load_engine(Query)) return; // we load the (existing) Prolog engine
    for(;;) {
      Object answer=M.get_answer(); // get an answer
      if(null==answer) {
        M.stop();
        break; // exit loop when finished
      }
      // print out an answer
      JavaIO.dump("testJinni: Prolog nondeterministic list append gives ===> "+answer);
    }
  }  
}  
  /**
   *  Exhibits work in Java on a term sent from Prolog
   */
  public static Object workOnPrologData(Object oterm) {
    //Machine machine=Top.new_machine();
   
    p("TERM===> "+oterm);
    Object O=((Fun)oterm).args[1];
    p("CHOP===> "+O);
    p("NODES===> "+countNodes(O));
    p("LEAVES===> "+getLeaves(O));
    
    Var X=new Var(1); Var Y=new Var(2);
    Object[] args={
      "hello",new Integer(11),
      X,X,Y,Y,
      new Double(3.14),
      "bye"
    };
    return new Fun("fromJava",args);
  } 
  
  /**
   * simple recursive count node method
   */
  static int countNodes(Object O) {
    p("count_trace: "+O);
    if(!(O instanceof Fun)) 
      return 1;
    Fun F=(Fun)O;
    int count=1;
    for(int i=0; i<F.args.length; 
      i++) {
      count+=countNodes(F.args[i]);
    }
    return count;
  }
  
  /**
   * collects leaves (Strings, ints etc.) of a Prolog term seen as tree
   */
  static Vector getLeaves(Object O) {
    Vector V=new Vector();
    try {
      getLeaves(O,V);
    }
    catch(Exception e) {
      JavaIO.errmes("error in getLeaves",e); 
    }
    return V;
  }
  
  /**
   * recurses over Prolog term seen as a tree
   */
  static void getLeaves(Object O,Vector V) {
    if(!(O instanceof Fun)) 
      V.addElement(O);
    else {
      Fun F=(Fun)O;
      for(int i=0; i<F.args.length; i++) {
        getLeaves(F.args[i],V);
      }
    }
  }
}
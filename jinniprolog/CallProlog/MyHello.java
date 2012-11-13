import prolog.kernel.*;
import java.util.*;
import java.io.*;
/**
 *  Shows examples of calls form Java to Prolog and back
 */
public class MyHello { 
 
  /**
   *  This class embeds Jinni into your application
   *  after initializing it from a .jc file created
   *  using the command "serialize(myfile)" that saves
   *  the state of a com piled application as  serialized
   *  Java object.
   *  To compile this type compile.bat, to run it type runhello.bat
   * 
   *  Note that new_machine() with no parameter starts with a default
   *  engine.
   */
  public static void main (String args[]) { 
    Machine M=Top.new_machine("hello.jc");
    M.run("println(type(go))");
    //Top.toplevel();
    JavaIO.halt(0);
  }
}
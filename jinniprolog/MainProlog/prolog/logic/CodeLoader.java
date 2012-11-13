package prolog.logic;
import java.io.LineNumberReader;
import java.io.FileReader;
import java.io.IOException;

public class CodeLoader implements Stateful {
  public static boolean load(String fname,CodeStore codeStore) {
    //return prolog.kernel.CodeIO.load(fname,codeStore); // ## if Machine/Machine
    Object[] args=new Object[]{fname,codeStore};
    Object R=call_java_class_method("prolog.kernel.CodeIO","load",args,true);
    if(R!=null) return true;
    return fload(fname, codeStore); // ## if LogicEngine
  }

  /**
   * Loads code from a local *.bp file
   */
  private static boolean fload(String fName, CodeStore codeStore) {
    Interact.println("WARNING: Code Loader restricted to  local *.bp files");
    boolean ok;
    try {
      LineNumberReader in = new LineNumberReader(new FileReader(fName));
      floadfromReader(in,codeStore);
      in.close();
      ok = true;
    }
    catch (Exception e) {
      Interact.errmes("Error in loading:" + fName, e);
      ok = false;
    }
    return ok;
  }

  /**
   * Reads instructions from *.bp Prolog bytecode files
   */
  private static void floadfromReader(LineNumberReader in, CodeStore codeStore) throws PrologException {
    int opcode, reg, arity;
    try {
      for (;;) {
        String l = in.readLine(); opcode = Integer.parseInt(l);
        l = in.readLine(); reg = Integer.parseInt(l);
        l = in.readLine(); arity = Integer.parseInt(l);
        l = in.readLine();
        codeStore.loadInstruction(opcode, reg, l, arity);
        if (opcode == CodeStore.END) break;
      }
      if (opcode != CodeStore.END)
        throw new LoadException("Premature end of file during instruction loading.");
    }
    catch (IOException e) {
      throw new LoadException("IOException during instruction loading.");
    }
  }

  /**
   * simple exact matching reflection layer
   */
  public static final Object call_java_class_method(String className,String methodName,Object[] args, boolean quiet) {
    Object result=null;
    int argn=args.length;
    Class[] argTypes=new Class[argn];
    for (int i=0;i<argn;i++) {
      argTypes[i]=args[i].getClass();
    }
    try {
      Class c = Class.forName(className);
      java.lang.reflect.Method m = c.getMethod(methodName,argTypes);
      result = m.invoke(c,args);
    }
    catch (Exception e) {
      if (!quiet) {
        System.err.println("reflection error: class="+className+",method="+methodName);
        e.printStackTrace();
      }
    }
    return result;
  }
}
package prolog.kernel;
import prolog.logic.*;

public class CodeIO implements Stateful {
  private CodeStore codeStore;

  public CodeIO(CodeStore codeStore) {
    this.codeStore=codeStore;
  }

  public static Boolean load(String fname,CodeStore codeStore) {
    boolean ok=new CodeIO(codeStore).try_load(fname);
    return new Boolean(ok);
  }

  /**
 * Loads a bytecode file from a URL or a file.
 * If this fails, tries to load byte code from
 * Java class.
 */
  synchronized boolean try_load(String fName) {
    if (jload()) return true;
    return fload(fName);
  }

  private boolean jload() {
    boolean ok = false;
    try {
      ok = Extender.activateBytecode(this.codeStore);
    }
    catch (Exception e) {
      Interact.errmes("Error loading Prolog bytecode from Java classes", e);
    }
    if(ok)
      Interact.println("Loading Prolog bytecode from Java classes.");
    return ok;
  }

  private boolean fload(String fName) {
    boolean ok;
    try {
      PrologReader in = JavaIO.toReader(fName);
      floadfromReader(in);
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
  public void floadfromReader(PrologReader in) throws PrologException {
    int opcode, reg, arity;

    for (; ; ) {
      String l = in.readln(); opcode = Integer.parseInt(l);
      l = in.readln(); reg = Integer.parseInt(l);
      l = in.readln(); arity = Integer.parseInt(l);
      l = in.readln();
      this.codeStore.loadInstruction(opcode, reg, l, arity);
      if (opcode == CodeStore.END) break;
    }
    if (opcode != CodeStore.END)
      throw new LoadException("Premature end of file during instruction loading.");
  }

  static final public void write_instr(PrologWriter out, int op, int reg, String name, int arity) {
    out.println(CodeStore.OUTPUT_INT(op));
    out.println(CodeStore.OUTPUT_INT(reg));
    out.println(CodeStore.OUTPUT_INT(arity));
    out.println(name);
  }

}

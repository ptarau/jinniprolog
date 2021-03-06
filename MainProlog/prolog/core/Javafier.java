package prolog.core;
import prolog.kernel.*;
import prolog.logic.*;

import java.io.*;
import java.lang.reflect.*;
import java.util.zip.*;

/**
Converts bytecode to Java classes that
can be included in jar files and autoloaded, for programs that
run from the Web or which, for some reason, should not depend on external files.
 */
public class Javafier {

  public static String TARGET = "Wam";
  public static String TARGET_DIR = "../prolog/kernel/";
  private static final short codeword = 27182; // for obfuscation

  private static boolean inactive = false;
  private static final boolean zipping = true; //

  public static final void turnOff() {
    inactive = true;
  }

  public static int SPLIT = 7200;

  public static void run() {
    javafy(Interact.PROLOG_BYTECODE_FILE);
  }

  public static void javafy(String ByteCodeFile) {
    Prolog.dump("Starting conversion of " + ByteCodeFile + " Prolog bytecode file to Java");
    try {
      javafy0(ByteCodeFile);
      Prolog.dump("conversion of " + ByteCodeFile + " to Java succeded");
    }
    catch (Throwable e) {
      JavaIO.errmes("conversion of " + ByteCodeFile + " to Java failed", e);
    }
  }

  public static byte[] toBytes(String fname) throws ExistenceException {
    try {
      return Transport.file2bytes(fname);
    }
    catch (IOException e) {
      throw new ExistenceException("unable to read: " + fname);
    }
  }

  private static void javafy0(String infile) throws PrologException {
    byte[] bytes = toBytes(infile);
    if (zipping) bytes = zip(bytes);
    short[] bs = fuse(bytes);

    PrologWriter out = null;

    int ctr = 0;
    int i = 0;
    for (; i < bs.length; i++) {
      if (0 == (i % SPLIT)) {
        // end class
        if (ctr > 0) out.println("};}");

        // new class Wam+ctr
        String sname = TARGET_DIR + TARGET + ctr + ".java";
        out = JavaIO.toWriter(sname);
        out.println("package prolog.kernel;\n");
        out.println("import prolog.logic.*;\n");
        out.println("\nclass " + TARGET + ctr + " implements Stateful {\nstatic short[] code={");
        ctr++;
      }

      int b = bs[i];
      if (0 != i % SPLIT) {
        out.println(",");
      }
      out.print("" + b);
    }

    if (0 != (i % SPLIT)) out.println("};}"); // unless already done
    out.close();

    // new class Wam

    String wname = TARGET_DIR + TARGET + ".java";
    out = JavaIO.toWriter(wname);
    out.println("package prolog.kernel;\n");
    out.println("import prolog.logic.*;\n");
    out.println("\npublic class " + TARGET + " implements Stateful {");

    out.println("public static final short[][] getByteCode() {");
    out.println("short[][] code=new short[maxwam][];");
    for (int j = 0; j < ctr; j++) {
      out.println("code[" + j + "]=" + TARGET + j + ".code;");
    }
    out.println("return code;\n}\n");

    out.println("final static public int maxwam=" + ctr + ";\n");

    out.println("final static public int prologVersion() {return " + Top.getPrologVersion() + ";}\n}");

    out.close();

    // end class
  }


  public static boolean activateBytecode(CodeStore codeStore) throws Exception {

    if (inactive) return false;

    short[][] scodes = null;

    try {
      String className = "prolog.kernel.Wam";
      String methodName = "getByteCode";
      Class C = Class.forName(className);
      Method theMethod = C.getMethod(methodName, (Class[])null);
      scodes = (short[][])theMethod.invoke(C, (Object[])null);
    }
    catch (Exception e) {
      //JavaIO.errmes("unable to activate Prolog bytecode from Java class",e);
    }

    if (null == scodes) return false;

    try {
      String className = "prolog.kernel.Wam";
      String methodName = "prologVersion";
      Class C = Class.forName(className);
      Method theMethod = C.getMethod(methodName, (Class[])null);
      Integer Version = (Integer)theMethod.invoke(C, (Object[])null);
      int vers = Version.intValue();
      int vers0=Top.getPrologVersion();
      if (vers0 != vers)
        JavaIO.warnmes("Prolog source version "+vers0+" different from runtime version: " + vers);
    }
    catch (Exception e) {
      JavaIO.warnmes("unable to get prolog version information->" + e);
    }

    int codesize = 0;
    for (int i = 0; i < scodes.length; i++) {
      codesize += scodes[i].length;
    }

    short[] scode = new short[codesize];

    int n = 0;
    for (int i = 0; i < scodes.length; i++) {
      short[] instr = scodes[i];
      for (int j = 0; j < instr.length; j++) {
        scode[n++] = instr[j];
      }
    }

    byte[] code = unfuse(scode);

    if (zipping) code = unzip(code);

    /*
    for(int k=0;k<code.length;) {
        int opcode=toInt(code[k++]);
        int reg=toInt(code[k++]);
        int arity=toInt(code[k++]);
        StringBuffer sbuf=new StringBuffer();
        for(;k<code.length;) {
          int x=toInt(code[k++]);
          if(0==x) break;
          sbuf.append((char)x);
        }  
        
        codeStore.loadInstruction(opcode, reg, sbuf.toString(), arity);
        if (opcode == 0) // Defs.END
          return true;
    }
    */

    try {
      PrologReader r = new PrologReader(new ByteArrayInputStream(code));
      (new CodeIO(codeStore)).floadfromReader(r);
      r.close();
      return true;
    }
    catch (IOException e) {
      JavaIO.warnmes("failed to load code from internal files, code length: " + code.length);
      return false;
    }
  }

  public static byte[] zip(byte[] bs) {
    Deflater zipper = new Deflater(); //Deflater.BEST_COMPRESSION);
    zipper.setInput(bs);
    zipper.finish();
    byte[] zs = new byte[bs.length];
    zipper.deflate(zs);
    int l = zipper.getTotalOut();
    //Prolog.dump("zip: "+l+"<"+bs.length);
    bs = new byte[l];
    System.arraycopy(zs, 0, bs, 0, l);

    return bs;
  }

  public static byte[] unzip(byte[] zs) {
    Inflater unzipper = new Inflater();
    byte[] cs = new byte[zs.length*10];
    try {
      unzipper.setInput(zs);
      unzipper.inflate(cs);
    }
    catch (DataFormatException e) {
      JavaIO.errmes("unable to get zipped Prolog bytecode from Java class", e);
      return null;
    }
    int l = unzipper.getTotalOut();
    byte[] bs = new byte[l];
    System.arraycopy(cs, 0, bs, 0, l);
    return bs;
  }

  private static final short xor(short A, short B) {
    return (short)((int)A ^ (int)B);
    //return A;
  }

  private static final short encode(short plain, short key) {
    return xor(plain, key);
  }

  private static final short decode(short cipher, short key) {
    return xor(cipher, key);
  }

  private static final short fuse(byte a, byte b) {
    short i = (short)((toInt(a) << 8) | toInt(b));
    return encode(i, codeword);
  }

  private static final byte[] unfuse(short i) {
    i = decode(i, codeword);
    byte[] unfused = new byte[2];
    unfused[0] = (byte)(((int)i) >> 8);
    unfused[1] = (byte)(((int)i) << 24 >> 24);
    return unfused;
  }

  private static final int toInt(byte b) {
    int x = b;
    if (x < 0) x = 256 + x;
    return x;
  }

  static final short[] fuse(byte[] bs) {
    int bl = bs.length;
    int cl = (0 == bl % 2) ? (bl / 2) : (bl + 1) / 2;
    short[] cs = new short[cl+1];
    cs[cl] = (short)(bl % 2);
    for (int i = 0; i < bs.length; i += 2) {
      byte a = bs[i];
      byte b = ((i + 1) < bs.length) ? bs[i + 1] : (byte)0;
      cs[i / 2] = fuse(a, b);
    }

    return cs;
  }

  static final byte[] unfuse(short[] cs) {
    int cl = cs.length;
    boolean odd = (1 == cs[cl - 1]);
    cl--;
    int bl = (odd) ? (cl * 2 - 1) : (cl * 2);
    byte[] bs = new byte[bl];
    for (int i = 0; i < cl; i++) {
      byte[] us = unfuse(cs[i]);
      bs[2 * i] = us[0];
      if (2 * i + 1 < bs.length) bs[2 * i + 1] = us[1];
    }

    return bs;
  }
}

package prolog.kernel;
import prolog.logic.*;
import java.io.*;

class GenericWriter extends PrintWriter {
  //
  public GenericWriter(OutputStream f) {
    //super(f,true); // flush automatically on NL
    super(tryEncoding(f,Top.defaultEncoding),true); // flush automatically on NL
  }
  
  public GenericWriter(String fileName) throws IOException {
    this(new FileOutputStream(fileName));
  }
 
  public GenericWriter() {
    super(JavaIO.getStdOutput(),true);
  }
  
  private static OutputStreamWriter tryEncoding(OutputStream outputStream,String encoding) {
   try {
     return new OutputStreamWriter(outputStream,encoding);
   }
   catch(Exception e) { // use default if it fails
      JavaIO.warnmes("failing to use character encoding: "+encoding);
      return new OutputStreamWriter(outputStream);
   }
  }
}

/**
 *  Basic Prolog char writer
 */

public class PrologWriter extends GenericWriter implements Stateful {
  /**
   * allows virtualizing output to go to something like a GUI element
   * clearly there's no file in this case - the extender should
   * provide some write and newLine methods that append to a window
   * it could also be used to do nothing at all - and disable any
   * output by implementing a "do nothing" PrologWriter extension
  */

  public TextSink textSink;

  public PrologWriter(TextSink textSink) {
    this.textSink=textSink;
  }

  public TextSink getTextSink() {
    return this.textSink;
  }
  
  public PrologWriter(OutputStream f) {
    super(f); 
  }
  
  public PrologWriter(String fileName) throws IOException {
   this(new FileOutputStream(fileName));
  }
  
  public final void super_write(int c) {
    if('\n'==c) super.flush();
    super.write((char)c);
  }

  public void write(int c) {
    //System.err.println(JavaIO.NL+this+"write => <"+(char)c+">");
    if(!JavaIO.showOutput) return;
    super_write(c);
    return;
  }
  
  public void flush() {
    if(!JavaIO.showOutput) return;
    super.flush();
    return;
  }
  
  public void print(String s) {
    //System.err.println(JavaIO.NL+this+"print => <"+s+">");
    if(!JavaIO.showOutput) return;
        super.print(s);
        if(s.indexOf('\n')>0) 
          super.flush();
    return;
  }
  
  public void println(String s) {
    print(s+JavaIO.NL);
  }
  public void println() {
    print(""+JavaIO.NL);
  }
  
}


package prolog.kernel;
import prolog.logic.*;
import java.io.*;

/**
 * Reads chars from byte streams using the current default encoding
 */
public class PrologClauseStringReader extends  PrologReader  {

  PrologClauseStringReader(InputStream stream) {
   super(stream);
  }
  
  PrologClauseStringReader(Reader reader) {
    super(reader);
  }
  
  public int get(Machine M) throws PrologException { 
    String c=nextClauseString();
    if(null==c) {
      stop(M);
      return 0;
    }
    else return M.prolog.atomTable.newFunctor(c,0);
  }
}
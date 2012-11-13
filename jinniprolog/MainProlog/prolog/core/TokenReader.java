package prolog.core;
import prolog.kernel.*;
import prolog.logic.*;
import java.io.*;

/**
 * Reads chars from char streams using the current default encoding
 */
public class TokenReader extends PrologReader {

  public static PrologReader toTokenReader(String fname) throws ExistenceException {
    return new TokenReader(JavaIO.url_or_file(fname));
  }

  public static PrologReader string2TokenReader(String cs) throws ExistenceException {
    Reader stream = JavaIO.string2reader(cs);
    return new TokenReader(stream);
  }

  public TokenReader(InputStream stream) {
    super(stream);
    init();
  }

  public TokenReader(Reader reader) {
    super(reader);
    init();
  }

  public void init() {
    tokenizer = new StreamTokenizer(this);
    tokenizer.parseNumbers();
    tokenizer.eolIsSignificant(true);
    tokenizer.ordinaryChar('.');
    //ordinaryChar('-'); // creates problems with -1 etc.
    tokenizer.ordinaryChar('/');
    //tokenizer.quoteChar('\'');
    //tokenizer.quoteChar('\"');
    tokenizer.ordinaryChar('\"');
    tokenizer.ordinaryChar('\'');
    tokenizer.ordinaryChar('-');
    tokenizer.wordChars('_', '_');
    //tokenizer.slashStarComments(true);
    //tokenizer.commentChar('%');
  }

  private StreamTokenizer tokenizer;

  public int get(Machine M) throws PrologException {
    if (null == tokenizer) return 0;

    int c = StreamTokenizer.TT_EOF;
    try {
      c = tokenizer.nextToken();
    }
    catch (IOException e) {
      JavaIO.errmes("tokenizer error", e);
    }

    int t;
    switch (c) {
      case StreamTokenizer.TT_WORD:
      t = M.prolog.atomTable.newFunctor(tokenizer.sval, 0); //better not internalize!
      break;

      case StreamTokenizer.TT_NUMBER:
      if (tokenizer.nval == Math.floor(tokenizer.nval))
        t = M.termReader.putInt((int)tokenizer.nval);
      else
        t = M.termReader.putFloat(tokenizer.nval);
      break;

      case StreamTokenizer.TT_EOL:
      t = M.prolog.atomTable.newFunctor("" + (char)10, 0);
      break;

      case StreamTokenizer.TT_EOF: {
        tokenizer = null;
        t = 0;
      }
      break;

      default: {
        String s = "" + (char)c;
        t = M.prolog.atomTable.newFunctor(s, 0);
      }

    }
    //Prolog.dump("<<"+c+">>"+M.termToString(t));
    return t;
  }

  public void stop(Machine M) {
    super.stop(M);
    tokenizer = null;
  }
}
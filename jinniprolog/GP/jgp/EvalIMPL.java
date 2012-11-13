package jgp;

import java.math.*;
import prolog.core.BigMath;

public class EvalIMPL extends Eval {
 
  public EvalIMPL(int nvars){
    super("=>",nvars);
  }
  
  /**
   * applies the primitive operation during synthesis
   */
  public BigInteger applyOp(BigInteger[] Bs) {    
     return impl(Bs[0],Bs[1]);
  }
}

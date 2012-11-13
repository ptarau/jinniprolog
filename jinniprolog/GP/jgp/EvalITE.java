package jgp;

import java.math.*;
import prolog.core.BigMath;

public class EvalITE extends Eval {
 
  public EvalITE(int nvars){
    super("ite",nvars);
  }
  
  
  public int getArity() {
    return 3;
  }
  
  /**
   * applies the primitive operation during synthesis
   */
  public BigInteger applyOp(BigInteger[] Bs) { 
     return ite(Bs[0],Bs[1],Bs[2]);
  }
}

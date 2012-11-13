package jgp;

import java.math.*;
import prolog.core.BigMath;

public class EvalHalfXOR extends Eval {
 
  public EvalHalfXOR(int nvars){
    super("<",nvars);
  }
  
  /**
   * applies the primitive operation during synthesis
   */
  public BigInteger applyOp(BigInteger[] Bs) {    
     return half_xor(Bs[0],Bs[1]);
  }
}

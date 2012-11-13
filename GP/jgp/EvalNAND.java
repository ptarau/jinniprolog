package jgp;

import java.math.*;
import prolog.core.BigMath;

public class EvalNAND extends Eval {
 
  public EvalNAND(int nvars){
    super("nand",nvars);
  }
  
  /**
   * applies the primitive operation during synthesis
   */
  public BigInteger applyOp(BigInteger[] Bs) {
     return nand(Bs[0],Bs[1]);
  }
}

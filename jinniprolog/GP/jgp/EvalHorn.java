package jgp;

import java.math.*;
import prolog.core.BigMath;

public class EvalHorn extends Eval {
 
  public EvalHorn(int nvars){
    super("ifand",nvars);
  }
  
  
  public int getArity() {
    return 3;
  }
  
  /**
   * applies the primitive operation during synthesis
   */
  public BigInteger applyOp(BigInteger[] Bs) { 
     BigInteger A=Bs[0];
     BigInteger B=Bs[1];
     BigInteger H=Bs[2];
     BigInteger C=A.and(B);
     return impl(C,H);
  }
}

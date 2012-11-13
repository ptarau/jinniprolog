package jgp;
import java.math.BigInteger;

public class HammingDist implements DistEval {
      /**
       * computes the Hamming distance between B an C
       */
       public final int distance(BigInteger B,BigInteger C) {
          BigInteger D=B.xor(C);
          return D.bitCount();
       }
}

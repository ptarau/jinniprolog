package jgp;
import java.math.BigInteger;

/**
 * Distance Evaluator: used to measure how close an individual 
 * from its intended model
 */
public interface DistEval {
    public int distance(BigInteger A,BigInteger B);
}

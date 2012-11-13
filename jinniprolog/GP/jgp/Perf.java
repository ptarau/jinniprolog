package jgp;

import java.math.BigInteger;

import prolog.core.BigMath;

/**
 * Implements an Individual that matches a spec
 * given as a model.
 */
public class Perf extends Ind {
	
  /**
   * crates a "perfect" individual by cloning "other"
   * (assuming that it has been tested that "other"
   * matches the spec given as its model)
   */
  public Perf(Ind other,GPWorld world) {
    super(other.getGeno(),world);
}
    
    /***
     * compares to Inds based on distance to their model
     * the closer the model the smaller the Ind is
     */
	public int compareTo(Object other) {
		Perf O = (Perf) other;
		return this.getGeno().compareTo(O.getGeno());
	}
}

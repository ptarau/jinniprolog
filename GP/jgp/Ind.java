package jgp;

import java.math.BigInteger;

/**
 * Implements an Individual evolving in a population of type GWorld.
 */
public class Ind implements Comparable {
	private BigInteger body; 
    private GPWorld world;
	
	
    /**
     * Gets the "genotype" describing this individual
     */
	public BigInteger getGeno() {
	   return body;
	}

    /**
     * Sets the "genotype" describing this individual to a given value
     */
	public void setGeno(BigInteger body) {
	   this.body=body;
	}
	
	/**
   * Creates an individual evolving towards a model.
   * 
   * @param model:
   *          target of the evolution
   * @param bits:
   *          size of the individual in bits
   * @param hops:
   *          lifespan in total changes or "hops"
   */ 
    public Ind(BigInteger body,GPWorld world) {
        setGeno(body);
        this.world=world;
    }
    
	public int hashCode() {
		return getGeno().hashCode();
	}

	public boolean equals(Object other) {
		if (other instanceof Ind)
			return getGeno().equals(((Ind) other).getGeno());
		else
			return false;
	}

    /***************************************************************************
     * compares to Inds based on distance to their model the closer the model
     * the smaller the Ind is
     */
	public int compareTo(Object other) {
		Ind O = (Ind) other;
		int distcomp=world.distanceToModel(this) - world.distanceToModel(O);
        //if(true) return distcomp;
        if(0!=distcomp) return distcomp;
        return getGeno().compareTo(O.getGeno());
	}
    
    /**
     * Induces a small random change in an individual. If the resulting
     * individual is "perfect" w.r.t to the model, it is returned, otherwise
     * null is returned.
     */
	public Ind mutate(boolean show) {
		int l = getGeno().bitLength();
		if (l > 0) {
			int n = GPWorld.rand.nextInt(l);
			setGeno(getGeno().flipBit(n));
			if(show) show("MUTATE");		
		}
		return this;
	}
	 
    public Ind negate(boolean show) {
        BigInteger geno=getGeno();
        setGeno(world.evaluator.negate(geno,world.gbits));
        if(show) show("NEGATE");
        return this;
    }
    
    /**
     * Implements self-similar growth
     */
    public Ind grow(boolean show) {
      BigInteger geno=getGeno();
      setGeno(world.evaluator.grow(geno,world.gbits));
      if(show) show("GROW");
      return this;
  }
    
	/**
   * Induces a small change in an individual trying to become more similar to
   * one closer to its model. If the resulting individual is "perfect" w.r.t to
   * the model, it is returned, otherwise null is returned.
   */
	public Ind imitate(Ind other,boolean show) {
		if(show) {
          other.show("\nOTHER=");
		  show("BEFORE");
        }
		BigInteger D = getGeno().xor(other.getGeno());
		int d = D.getLowestSetBit();
		// System.out.println("d=" + d + ",D=" + D.toString(2));
		setGeno(getGeno().flipBit(d));
		if(show) show("AFTER=");
	    return this;
	}
	
	public boolean isPerfect() {
		return world.model.equals(world.evalPheno(this));
	}

	/**
   * returns a (bit) String representation and pads on the left with zeroe if
   * needed on the left
   * 
   * @param B:
   *          a BigInteger to be shown as a bitstring
   * @param bits:
   *          width of the result
   */
	public static String big2string(BigInteger B,int bits) {
		String s=B.toString(2);
		StringBuffer b=new StringBuffer(bits);
		int n=bits-s.length();
		while(--n >=0) {
			b.append('0');
		}
		b.append(s);
		return b.toString();
	}
		
	/**
   * returns a String representation of this
   */
	public String toString() {
		StringBuffer b=new StringBuffer();
		b.append("d");
		b.append(world.distanceToModel(this));
		b.append("_");
	    b.append(big2string(getGeno(),world.gbits));
	    b.append("->");
	    b.append(big2string(world.evalPheno(this),1<<world.nvars));
		return b.toString();
	}

	/**
   * prints out a string representation of this individual
   */
    
	public void show(String op) {
	  System.out.println(op + "=>" + this);
	}
}

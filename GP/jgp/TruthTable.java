package jgp;

import java.math.BigInteger;

import prolog.core.Cat;
import prolog.logic.Stateful;

class TruthTable implements Stateful {
  public int nvars;
  public long tt;
  
  public int hashCode() {
    return nvars+(int)tt;
  }
  
  public boolean equals(Object that) {
    if(!(that instanceof TruthTable)) return false;
    TruthTable TT=(TruthTable)that;
    return (TT.nvars==nvars) && (TT.tt==tt);
  }
  
  public TruthTable(int nvars,long tt) {
    this.nvars=nvars;
    this.tt=tt;
  }
  
  public String toString() {
    return nvars+":"+Ind.big2string(BigInteger.valueOf(tt),1<<nvars);
  }
  
}

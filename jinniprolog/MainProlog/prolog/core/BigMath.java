package prolog.core;

// TODO: use MutableBigIntegers ??

import prolog.kernel.*;
import prolog.logic.*;

import java.math.*;

//import java.util.BitSet;

/**
 * Provides some utility functions on infinit length numbers
 */
public class BigMath implements Stateful {
  public static final BigInteger zero=BigInteger.valueOf(0);

  public static final BigInteger one=BigInteger.valueOf(1);

  /**
   * G[i]=B[i+1] xor B[i]
   */
  public static BigInteger bigint2gray(BigInteger B) {
    // int l=B.bitLength();
    BigInteger B1=B.shiftRight(1);
    B1=B.xor(B1);
    // if(B.testBit(l)) B1.setBit(l); else B1.clearBit(l);
    return B1;
  }

  /**
   * B[i]=B[i+1] xor G[i]
   */
  public static BigInteger gray2bigint(BigInteger G,int l) {
    // int l=G.bitLength();
    if(0==l)
      return zero;
    char[] S=new char[l];
    --l;
    char b;
    /*
     * if(G.testBit(l)) b='1'; else b='0'; S[l]=b;
     * 
     * //System.out.print("["+l+"]"+b); for (int i=l-1;i>=0;i--) {
     * if(G.testBit(i)!=(b=='1')) b='1'; else b='0'; S[i]=b;
     * //System.out.print("<"+(i)+">"+b); }
     */
    if(G.testBit(l))
      b='1';
    else
      b='0';
    S[0]=b;

    // System.out.print("["+l+"]"+b);
    for(int i=l-1;i>=0;i--) {
      if(G.testBit(i)!=(b=='1'))
        b='1';
      else
        b='0';
      S[l-i]=b;
      // System.out.print("<"+(i)+">"+b);
    }

    String SB=new String(S);
    // System.out.println("------"+SB);

    return new BigInteger(SB,2);
  }

  public static BigInteger bigones(int nbits) {
    return one.shiftLeft(nbits).subtract(one);
  }

  public static BigInteger bignot(int nbits,BigInteger B) {
    return bigones(nbits).andNot(B);
  }
  
  public static BigInteger lvar2bigint(int maxvar,int nvar) {
    BigInteger mask=bigones(1<<maxvar);
    int nk=maxvar-(nvar+1);
    BigInteger D=(one.shiftLeft(1<<nk)).add(one);
    BigInteger R=mask.divide(D);
    return(R);
  }
  
  public static BigInteger rvar2bigint(int maxvar,int nvar) {
    if(nvar>=maxvar) return null;
    BigInteger mask=bigones(1<<maxvar);
    int nk=nvar+1;
    BigInteger D=(one.shiftLeft(1<<nk)).add(one);
    BigInteger R=mask.divide(D);
    return(R);
  }
  
  public static Cat ints2cat(int arity,int max) {
    Cat C=new Cat();
    BigInteger Max=new BigInteger(""+max);
    C.setProp(zero,"v","l");
    C.setHyper(zero,1);
    C.setProp(one,"v","l");
    C.setHyper(one,1);
    C.setProp(Max,"v","r");
    C.setHyper(Max,3);
    try {
      for(int i=0;i<=max;i++) {
        BigInteger I=BigInteger.valueOf(i);
        // C.setMorphism("*",I,"x","i");
        addTuplesToCat(C,one.add(one),arity,I);
      }
    } catch(Exception e) {
      e.printStackTrace();
    }
    // Prolog.dump("$$$ints2cat:\n"+C);
    return C;
  }

  public static Cat bigint2cat(int arity,BigInteger N) {
    return bigint2cat(2,arity,N);
  }
  
  public static Cat bigint2cat(int maxur,int arity,BigInteger N) {
    Cat C=new Cat();
    /*
    C.setProp(zero,"v","l");
    C.setHyper(zero,1);
    */
    for(int i=0;i<maxur;i++) {
      BigInteger I=BigInteger.valueOf(i);
      C.setProp(I,"v","l");
      C.setHyper(I,1);
    }
    C.setProp(N,"v","r");
    C.setHyper(N,3);
    BigInteger MaxUr=BigInteger.valueOf(maxur);
    try {
      addTuplesToCat(C,MaxUr,arity,N);
    } catch(Exception e) {
      e.printStackTrace();
    }

    // Prolog.dump("$$$bigint2cat:\n"+C);
    return C;
  }

  private static void addTuplesToCat(Cat C,BigInteger MaxUr,int arity,BigInteger N) {
    // Prolog.dump("addin bigint="+N);
    /*
    if(N.equals(zero))
      return;
    if(N.equals(one))
      return;
    */
    if(N.compareTo(MaxUr)<0) return;
    // Prolog.dump("getProp="+C.getProp(N,"v")+"N="+N);
    if(!"r".equals(C.getProp(N,"v"))) {
      C.setProp(N,"v","v");
      C.setHyper(N,0);
    }
    BigInteger[] Ns=bigint2tuple(arity,N);
    // Prolog.dump("tuple Ns:"+tupleToString(Ns));
    int l=Ns.length;
    for(int i=0;i<l;i++) {
      BigInteger M=Ns[i];
      // Prolog.dump("bigint2cat M:"+M);
      if(!"l".equals(C.getProp(M,"v"))) {
        C.setProp(M,"v","v");
        C.setHyper(M,0);
      }
      String mid=N+":"+i;
      C.setProp(mid,"v","i");
      C.setHyper(mid,2);
      C.setMorphism(N,mid,"f","m");
      C.setMorphism(mid,M,"m","t");
      addTuplesToCat(C,MaxUr,arity,M);
    }
  }
  
  /**
   * Converts big integer to n-tuple of big integers as a bijection from N to
   * Nx...N.
   */
  public static BigInteger[] bigint2tuple(int arity,BigInteger N) {
    BigInteger[] Tuple=new BigInteger[arity];
    for(int i=0;i<arity;i++) {
      Tuple[i]=zero;
    }
    int l=N.bitLength();
    for(int k=0;k<l;k++) {
      boolean bit=N.testBit(k);
      if(!bit)
        continue;
      // int m=arity-1-(k%arity);
      int m=k%arity;
      int d=k/arity;
      BigInteger I=Tuple[m];
      Tuple[m]=I.setBit(d);
    }
    return Tuple;
  }

  /**
   * Converts an n-tuple of big integers to a single big integer as a bijection
   * from Nx...xN to N.
   */
  public static BigInteger tuple2bigint(BigInteger[] Tuple) {
    int arity=Tuple.length;
    BigInteger N=zero;
    for(int m=0;m<arity;m++) {
      BigInteger M=Tuple[m];
      int l=M.bitLength();
      for(int d=0;d<l;d++) {
        boolean bit=M.testBit(d);
        if(!bit)
          continue;
        int k=arity*d+m;
        N=N.setBit(k);
      }
    }
    return N;
  }

  /**
   * Converts a big integer seen as a bitstring subset to a set of integers
   * indicating its 0's or 1's locations, depending on selectOnes being false or true
   */
  public static BigInteger[] bigint2pos(int l,BigInteger N,boolean selectOnes) {
    ObjectStack V=new ObjectStack();
    for(int k=0;k<l;k++) {
      boolean bit=N.testBit(k);
      if(bit!=selectOnes)
        continue;
      BigInteger K=BigInteger.valueOf(k);
      V.push(K);
    }
    int n=V.size();
    BigInteger[] Tuple=new BigInteger[n];
    for(int i=0;i<n;i++) {
      Tuple[i]=(BigInteger)V.at(i);
    }
    return Tuple;
  }

  /**
   * Converts a big integer seen as a bitstring subset to set of integers
   * indicating its 1's locations.
   */
  public static BigInteger[] bigint2exps(BigInteger N) {
    return bigint2pos(N.bitLength(),N,true);
  }
  
  /**
   * Converts a big integer seen as a bitstring subset to set of integers
   * indicating its 0's locations.
   */
  public static BigInteger[] bigint2nexps(int l,BigInteger N) {
    return bigint2pos(l,N,false);
  }
  
  /**
   * Converts a set of bit locations seen as powers of 2 to the big integer
   * obtained as their sum - or seen as a bitstring subset with 1's at the bit
   * locations indicated by the set.
   */
  public static BigInteger exps2bigint(BigInteger[] Tuple) {
    int arity=Tuple.length;
    BigInteger N=zero;
    for(int m=0;m<arity;m++) {
      BigInteger Exp=Tuple[m];
      int exp=Exp.intValue();
      N=N.setBit(exp);
    }
    return N;
  }

  /*
   * public static int[] bigint2ipairs(BigInteger B) { BigInteger[]
   * Bs=bigint2exps(B); int l=Bs.length; int[] is=new int[2*l]; for(int i=0;i<l;l++) {
   * BigInteger X=Bs[i]; BigInteger[] Xs=bigint2tuple(2,X); int
   * a=Xs[0].intValue(); int b=Xs[1].intValue(); is[2*i]=a; is[2*i+1]=b; }
   * return is; }
   */

  public static Cat bigint2igraph(BigInteger B) {
    BigInteger[] Bs=bigint2exps(B);
    int l=Bs.length;
    Cat C=new Cat();
    // Prolog.dump("l="+l);
    for(int i=0;i<l;i++) {
      BigInteger X=Bs[i];
      // Prolog.dump("i="+i+":"+X);
      BigInteger[] Xs=bigint2tuple(2,X);
      Integer F=new Integer(Xs[0].intValue());
      Integer T=new Integer(Xs[1].intValue());
      C.setProp(F,"x","a");
      C.setHyper(F,0);
      C.setProp(T,"x","a");
      C.setHyper(T,0);
      C.setMorphism(F,T,"f","t");
    }

    int max=bigint2n(B);
    // Prolog.dump("max"+max);
    for(int i=0;i<max;i++) {
      Integer I=new Integer(i);
      if(null==C.getProp(I,"x")) {
        C.setProp(I,"x","a");
        C.setHyper(I,1);
      }
    }
    return C;
  }

  public static BigInteger igraph2bigint(Cat C) {
    ObjectIterator Vs=C.vertexIterator();
    ObjectStack Es=new ObjectStack();
    while(Vs.hasNext()) {
      Object V=Vs.next();
      int v=((Integer)V).intValue();
      ObjectIterator Ts=C.outIterator(V);
      while(Ts.hasNext()) {
        Object T=Ts.next();
        int t=((Integer)T).intValue();
        BigInteger E=pair2bigint(v,t);
        Es.push(E);
      }
    }
    Object[] Os=Es.toArray();
    int l=Os.length;
    BigInteger[] Exps=new BigInteger[l];

    for(int i=0;i<l;i++) {
      Exps[i]=(BigInteger)Os[i];
    }

    return exps2bigint(Exps);
  }

  public static BigInteger pair2bigint(int x,int y) {
    BigInteger[] tuple=new BigInteger[2];
    tuple[0]=BigInteger.valueOf(x);
    tuple[1]=BigInteger.valueOf(y);
    return tuple2bigint(tuple);
  }

  public static int bigint2n(BigInteger B) {
    int l=B.bitLength();
    int r=(int)Math.sqrt((double)l)+1;
    // BigInteger R=new BigInteger.valueOf(r);return R;
    return r;
  }

  public static BigInteger maxBigOf(Cat C) {
    BigInteger Max=zero;
    ObjectIterator Vs=C.vertexIterator();
    while(Vs.hasNext()) {
      Object V=Vs.next();
      BigInteger B;
      if(V instanceof BigInteger)
        B=(BigInteger)V;
      else {
        String S=(String)V;
        S=Machine.unQuote(S);
        B=new BigInteger(S);
      }
      if(Max.compareTo(B)<0)
        Max=B;
    }
    return Max;
  }

  public static String tupleToString(BigInteger[] Tuple) {
    StringBuffer buf=new StringBuffer("[");
    for(int i=0;i<Tuple.length;i++) {
      if(i>0)
        buf.append(",");
      buf.append(Tuple[i].toString());
    }
    buf.append("]");
    return buf.toString();
  }

  public static void test() {
    try {
      BigInteger N=new BigInteger("12345678901234567890");
      /*
       * //BigInteger N=new BigInteger("12345");
       * 
       * BigInteger[] Es=bigint2exps(N); Prolog.dump("#Es="+tupleToString(Es));
       * BigInteger NE=exps2bigint(Es); Prolog.dump("EXPS:"+N+"="+NE);
       * 
       * BigInteger[] Ns=bigint2tuple(16,N);
       * Prolog.dump("#Ns="+tupleToString(Ns)); BigInteger NN=tuple2bigint(Ns);
       * Prolog.dump("TUPLES:"+N+"="+NN);
       */

      Cat C=bigint2igraph(N);
      Prolog.dump(N+"=>"+C);
    } catch(Exception e) {
      e.printStackTrace();
    }
  }

}

/*
 * % derived operations - see BigInteger API for semantics % most of them work
 * by calling their names in class BigInteger % some, involving special class
 * parameters may need adaptations - see for instance Random
 * 
 * BigInteger abs() Returns a BigInteger whose value is the absolute value of
 * this BigInteger. BigInteger add(BigInteger val) Returns a BigInteger whose
 * value is (this + val). BigInteger and(BigInteger val) Returns a BigInteger
 * whose value is (this & val). BigInteger andNot(BigInteger val) Returns a
 * BigInteger whose value is (this & ~val). int bitCount() Returns the number of
 * bits in the two's complement representation of this BigInteger that differ
 * from its sign bit. int bitLength() Returns the number of bits in the minimal
 * two's-complement representation of this BigInteger, excluding a sign bit.
 * BigInteger clearBit(int n) Returns a BigInteger whose value is equivalent to
 * this BigInteger with the designated bit cleared. int compareTo(BigInteger
 * val) Compares this BigInteger with the specified BigInteger. BigInteger
 * divide(BigInteger val) Returns a BigInteger whose value is (this / val).
 * BigInteger[] divideAndRemainder(BigInteger val) Returns an array of two
 * BigIntegers containing (this / val) followed by (this % val). double
 * doubleValue() Converts this BigInteger to a double. boolean equals(Object x)
 * Compares this BigInteger with the specified Object for equality. BigInteger
 * flipBit(int n) Returns a BigInteger whose value is equivalent to this
 * BigInteger with the designated bit flipped. float floatValue() Converts this
 * BigInteger to a float. BigInteger gcd(BigInteger val) Returns a BigInteger
 * whose value is the greatest common divisor of abs(this) and abs(val). int
 * getLowestSetBit() Returns the index of the rightmost (lowest-order) one bit
 * in this BigInteger (the number of zero bits to the right of the rightmost one
 * bit). int hashCode() Returns the hash code for this BigInteger. int
 * intValue() Converts this BigInteger to an int. boolean isProbablePrime(int
 * certainty) Returns true if this BigInteger is probably prime, false if it's
 * definitely composite. long longValue() Converts this BigInteger to a long.
 * BigInteger max(BigInteger val) Returns the maximum of this BigInteger and
 * val. BigInteger min(BigInteger val) Returns the minimum of this BigInteger
 * and val. BigInteger mod(BigInteger m) Returns a BigInteger whose value is
 * (this mod m). BigInteger modInverse(BigInteger m) Returns a BigInteger whose
 * value is (this-1 mod m). BigInteger modPow(BigInteger exponent, BigInteger m)
 * Returns a BigInteger whose value is (thisexponent mod m). BigInteger
 * multiply(BigInteger val) Returns a BigInteger whose value is (this * val).
 * BigInteger negate() Returns a BigInteger whose value is (-this). BigInteger
 * nextProbablePrime() Returns the first integer greater than this BigInteger
 * that is probably prime. BigInteger not() Returns a BigInteger whose value is
 * (~this). BigInteger or(BigInteger val) Returns a BigInteger whose value is
 * (this | val). BigInteger pow(int exponent) Returns a BigInteger whose value
 * is (thisexponent). static BigInteger probablePrime(int bitLength, Random rnd)
 * Returns a positive BigInteger that is probably prime, with the specified
 * bitLength. BigInteger remainder(BigInteger val) Returns a BigInteger whose
 * value is (this % val). BigInteger setBit(int n) Returns a BigInteger whose
 * value is equivalent to this BigInteger with the designated bit set.
 * BigInteger shiftLeft(int n) Returns a BigInteger whose value is (this << n).
 * BigInteger shiftRight(int n) Returns a BigInteger whose value is (this >> n).
 * int signum() Returns the signum function of this BigInteger. BigInteger
 * subtract(BigInteger val) Returns a BigInteger whose value is (this - val).
 * boolean testBit(int n) Returns true if and only if the designated bit is set.
 * byte[] toByteArray() Returns a byte array containing the two's-complement
 * representation of this BigInteger. String toString() Returns the decimal
 * String representation of this BigInteger. String toString(int radix) Returns
 * the String representation of this BigInteger in the given radix. static
 * BigInteger valueOf(long val) Returns a BigInteger whose value is equal to
 * that of the specified long. BigInteger xor(BigInteger val) Returns a
 * BigInteger whose value is (this ^ val).
 */


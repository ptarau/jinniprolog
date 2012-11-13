package prolog.logic;

/**
 *  Implements external representations of Prolog
 *  compound terms - a functor of the form Symbol / Arity
 *  Uses full hashing on all arguments so if that if is GROUND 
 *  it can be used as a key in an ObjectDict
 */
public class Fun implements Stateful {
  public Fun(String name,Object[] args) {
    this.name=name;
    this.args=args;
  }

  public Fun(String name,int n) {
    this.name=name;
    this.args=new Object[n];
    //for(int i=0;i<n;i++) {
    //  
    //}
  }
   
  public Fun(String name,Object a1) {
    this.name=name;
    this.args=new Object[1];
    args[0]=a1;
  }
   
  public Fun(String name,Object a1,Object a2) {
    this.name=name;
    this.args=new Object[2];
    args[0]=a1;
    args[1]=a2;
  }
  
  public Fun(String name,Object a1,Object a2,Object a3) {
    this.name=name;
    this.args=new Object[3];
    args[0]=a1;
    args[1]=a2;
    args[2]=a3;
  }
  
  public Fun(String name,Object a1,Object a2,Object a3,Object a4) {
    this.name=name;
    this.args=new Object[4];
    args[0]=a1;
    args[1]=a2;
    args[2]=a3;
    args[3]=a4;
  }
  
  public Fun(String name,Object a1,Object a2,Object a3,Object a4,Object a5) {
    this.name=name;
    this.args=new Object[5];
    args[0]=a1;
    args[1]=a2;
    args[2]=a3;
    args[3]=a4;
    args[4]=a5;
  }
  
  public Fun(String name,Object a1,Object a2,Object a3,Object a4,Object a5,Object a6) {
    this.name=name;
    this.args=new Object[6];
    args[0]=a1;
    args[1]=a2;
    args[2]=a3;
    args[3]=a4;
    args[4]=a5;
    args[5]=a6;
  }

  final public String name;
  final public Object[] args;
  
  /** gets arg in 1..arity range */
  public Object getArg(int i) {
    return this.args[i-1];
  }
  
  /** sets arg in in 1..arity range */
  public void setArg(int i,Object A) {
    this.args[i-1]=A;
  }

  public int hashCode() {
    return args.length<<8+name.hashCode();
  }

  public boolean equals(Object O) {
    if (!(O instanceof Fun)) return false;
    Fun F=(Fun)O;
    return args.length==F.args.length && name.equals(F.name);
  }

  public int deepHashCode() {
    int l=args.length;
    int k=name.hashCode()^(l<<2);
    for (int i=0;i<l;i++) {
      Object X=args[i];
      int j;
      if (null==X) { j=0; Interact.warnmes("null arg in Fun="+name+"/"+l+":"+(i+1)); }
      else 
        j=(X instanceof Fun)?((Fun)X).deepHashCode():X.hashCode();
      k+=(k<<4)+j;
    }
    return k;
  }

  public boolean deepEquals(Object O) {
    if(!(O instanceof Fun)) return false;
    Fun F=(Fun)O;
    if(!(args.length==F.args.length && name.equals(F.name))) return false;
    for(int i=0; i<args.length; i++) {
      if(args[i] instanceof Fun && F.args[i] instanceof Fun) {
        if(!((Fun)args[i]).deepEquals((Fun)(F.args[i]))) return false;
      }
      else if(!args[i].equals(F.args[i])) return false;
    }
    return true;
  }
  
  public String toString() {
    StringBuffer buf=new StringBuffer();
    if(args.length==2 && ("/".equals(name) || "-".equals(name))) {
      buf.append("(");
      buf.append(args[0].toString());
      buf.append(" "+name);
      buf.append(args[1].toString());
      buf.append(")");
    }
    else {
      buf.append(TermConverter.toQuoted(name));
      buf.append("(");
      for(int i=0; i<args.length; i++) {
        Object o=args[i];
        String s;
        if(null==o) s="\'$null\'";
        else s=o.toString();
        buf.append(s);
        if(i<args.length-1) buf.append(",");
      }
      buf.append(")");
    }
    return buf.toString();
  }
}
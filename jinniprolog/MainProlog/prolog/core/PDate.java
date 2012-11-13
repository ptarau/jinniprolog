package prolog.core;
import prolog.logic.Fun;
import prolog.logic.Stateful;
import java.util.*;

// meant to do arithmetics on things like 2003/04/01 08:02:00

public class PDate extends GregorianCalendar implements Stateful {
  
  public static Fun inc_date(Fun Date,Fun Inc) {
   
    PDate P=new PDate(Date);
    
    //System.out.println(Date+"=>PDate="+P);
    
    int Y=((Integer)Inc.getArg(1)).intValue();
    int M=((Integer)Inc.getArg(2)).intValue();
    int D=((Integer)Inc.getArg(3)).intValue();
    int H=((Integer)Inc.getArg(4)).intValue();
    int N=((Integer)Inc.getArg(5)).intValue();
    int S=((Integer)Inc.getArg(6)).intValue();
    
    P.add(Calendar.YEAR,Y);
    P.add(Calendar.MONTH,M);
    P.add(Calendar.DAY_OF_MONTH,D);
    P.add(Calendar.HOUR_OF_DAY,H);
    P.add(Calendar.MINUTE,N);
    P.add(Calendar.SECOND,S);
    
    //System.out.println("PDate.result=>"+R);
    
    return P.toFun();
  }
  
  public static String format_date(Fun date) {
    return new PDate(date).toString();
  }
  
  public PDate(int year, int month, int dayOfMonth, int hourOfDay, int minute,int sec) {
     super(year, month, dayOfMonth, hourOfDay, minute,sec);
  }
  
  public PDate(Fun date) {
    this( 
      ((Integer)date.getArg(1)).intValue(),
      ((Integer)date.getArg(2)).intValue(),
      ((Integer)date.getArg(3)).intValue(),
      ((Integer)date.getArg(4)).intValue(),
      ((Integer)date.getArg(5)).intValue(),
      ((Integer)date.getArg(6)).intValue()
    );
  }
  
  public Fun toFun() {
    int Y=get(Calendar.YEAR);
    int M=get(Calendar.MONTH);
    int D=get(Calendar.DAY_OF_MONTH);
    int H=get(Calendar.HOUR_OF_DAY);
    int N=get(Calendar.MINUTE);
    int S=get(Calendar.SECOND);
    
    Fun R=new Fun("date",
        new Integer(Y),
        new Integer(M),
        new Integer(D),
        new Integer(H),
        new Integer(N),
        new Integer(S)
     );
    
     return R;
  }
  
  public String toString() {
    StringBuffer b = new StringBuffer();
    b.append(get(Calendar.YEAR));b.append("/");
    b.append(get(Calendar.MONTH));b.append("/");
    b.append(get(Calendar.DAY_OF_MONTH));b.append(" ");
    b.append(get(Calendar.HOUR_OF_DAY));b.append(":");
    b.append(get(Calendar.MINUTE));b.append(":");
    b.append(get(Calendar.SECOND));
    return b.toString();
  }
  
}

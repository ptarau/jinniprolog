package prolog.kernel;

import prolog.logic.*;


public class Wam implements Stateful {
public static final short[][] getByteCode() {
short[][] code=new short[maxwam][];
code[0]=Wam0.code;
code[1]=Wam1.code;
code[2]=Wam2.code;
code[3]=Wam3.code;
code[4]=Wam4.code;
code[5]=Wam5.code;
return code;
}

final static public int maxwam=6;

final static public int prologVersion() {return 2;}
}

package prolog.logic;

/**
  Implements everything needed for a Prolog code container - in particular a Prolog class
*/
public final class CodeStore extends Defs {

  final String NAME(int s) { return atomTable.getAtomName(s); };

  /*******INTERFACE INSTRUCTION OPERATIONS ********************/

  static final int GETINSTR_OP(int instr) { return RCGET(instr); }

  private final void SETOP(int instPtr,int val) { code[instPtr]=RCPUT(code[instPtr],val); }
  private final int GETOP(int instPtr) { return RCGET(code[instPtr]); }
  private final void SETREG(int instPtr,int val) { code[instPtr]=LCPUT(code[instPtr],val); }
  private final int GETREG(int instPtr) { return LCGET(code[instPtr]); }
  private final void SETLEFT(int instPtr,int val) { code[instPtr]=MCPUT(code[instPtr],val); }
  private final int GETARG(int instPtr) { return MCGET(code[instPtr]); }
  final int GETINSTR(int instrPtr,int step) { return code[instrPtr+step]; }

  private final int GETFUN(int instr) { return code[instr+1]; }
  private final int SETFUN(int instr,int val) {
    instr++;
    code[instr]=val;
    return instr;
  }

  final int GETLABEL(int instr) { return code[instr+1]; }

  private final void SETLABEL(int instr,int val) { code[instr+1]=val; }

  private final boolean COMPRESS(int reg,int Simple,int Double,int First,int Triple) {
    //return true;

    if (1==prevLen&&Simple==GETOP(codeTop-1)) {
      SETOP(codeTop-1,Double);
      codeTop--;
      SETLEFT(codeTop,reg);
      if (2==prevPrevLen&&First==GETOP(codeTop-2)) {
        SETOP(codeTop-2,Triple);
      }
      return true;
    }
    return false;

  }

  private final boolean OCOMPRESS(int reg,int Simple,int Double,int First,int Triple) {
    return COMPRESS(reg,Simple,Double,First,Triple);
  }

  //==========================================
  private Prolog prolog;
  private Dict dict;
  private AtomTable atomTable;
  private int codeTop;
  private int codeTopBak;
  private int codeTopBakLOADTIME;
  private int curPred;
  private int prevInstr; // last instruction.
  private int prevLen;
  private int prevPrevLen;
  private int[] code;
  private static final byte[] instructionLength=initInstructionLengths();

  CodeStore(Prolog prolog,int codeMax)  throws PrologException {
    this.prolog=prolog;

    this.dict=prolog.dict;
    this.atomTable=prolog.atomTable;

    this.code=new int[codeMax];
    //initInstructionLengths();

    // prime instruction stack.
    SETOP(codeTop,END);
    codeTop+=getInstructionLength(END);
    curPred=prolog.G_true;
    prevPrevLen=prevLen=2;
    SETOP(codeTop,PROCEED);
    dict.setpred(prolog.G_true,codeTop);
    codeTop=SETFUN(codeTop,prolog.G_true);
    codeTop++;
  }

  protected CodeStore cloneWith(Prolog prolog) throws CloneNotSupportedException {
    CodeStore child=(CodeStore)clone();
    child.code=(int[])code.clone();
    child.atomTable=prolog.atomTable;
    child.dict=prolog.dict;
    return child;
  }

  int getTop() { return codeTop; }

  /** Return the count of entries.*/
  final int getUsed() { return codeTop+1; }

  int getMax() { return code.length; }

  int getFree() { return code.length-codeTop-1; }

  // get index of prolog_main
  int getStartPoint() throws PrologException {
    int start=prolog.G_empty;
    //try {
    start=dict.getpred(prolog.G_prolog_main);
    //}
    //catch (PrologException ignore) {
    //}
    if (start==prolog.G_empty)
      throw new ExistenceException(Interact.NL+"Fatal error: no definition for start point predicate.");
    return start;
  }

  void setTopBack() {
    //Prolog.dump("setTopBack: "+codeTopBak+"<-"+codeTop);
    codeTopBak=codeTop;
  }

  void rollback() { codeTop=codeTopBak; }

  void resetTopBak() {
    //Prolog.dump("reset code top:"+codeTop+">="+codeTopBak+">="+codeTopBakLOADTIME);
    codeTopBak=codeTopBakLOADTIME;
  }


  /**
   * Loads a bytecode file from a URL or a file
   */
  boolean load(String fName) {
    //Interact.println("   begin loading:" + fName);

    boolean ok=CodeLoader.load(fName,this);

    //Interact.println("   finished loading.");
    setTopBack();
    if (Prolog.LOADTIME==prolog.timeStamp) { // should be always the case
      codeTopBakLOADTIME=codeTop;
      //Prolog.dump("codeTopBackLOADTIME="+codeTopBakLOADTIME);
    }
    return ok;
  }


  final private void resize_to(int realsize) {
    //int l=code.length;
    //Prolog.dump("@"+code.hashCode()+"resizing at:"+codeTop+":("+l+"):"+realsize);
    int[] newstack=new int[realsize];
    System.arraycopy(code,0,newstack,0,codeTop+1);
    this.code=newstack;
    if (PrologGC.trace>=2) Prolog.dump("@"+this.code.hashCode()+"=@"+newstack.hashCode()+"resized:"+codeTop+":"+code.length);
  }

  final private void expand() {
    if (codeTop+100>code.length) resize_to(code.length<<1);
  }

  final private void shrink() {
    //if(codeTop+100<code.length>>1) resize_to(code.length>>1);
    //if(Prolog.timeStamp>Prolog.LOADTIME) resize_to(codeTop+2);
    if (prolog.timeStamp>Prolog.LOADTIME&&codeTop+100<code.length>>1) resize_to(codeTop+2);
  }

  private boolean skipClause=false;

  public final void loadInstruction(int op,int reg,String name,int arity) throws PrologException {
    expand();

    //Prolog.dump("x"+op+","+reg+","+arity+","+name+"!!!");
    //Interact.halt("end"); 

    if (null==name)
      Interact.warnmes("unexpected instr:"+codeTop+
         " Op:"+op+"-->"+getInstructionName(op)+
         " reg:"+reg+" "+name+"/"+arity);

    // $$ high risk change - ignores clauses when not grouped
    if (skipClause && op!=CLAUSE) {
      // Prolog.dump("skiping op="+name); // $$
      return;
    }

    SETOP(codeTop,op);
    switch (op) {
      case CLAUSE: {
          skipClause=false; //$$
          int pred=inputTerm(name,arity);
          boolean isNewPred=dict.setpred(pred,codeTop);
          if (curPred!=prolog.G_true)
            SETLABEL(prevInstr,codeTop);
          reg++;
          //System.err.println("Prev instr:" + prevInstr);
          if (isNewPred) {
            if (curPred!=prolog.G_true&&GETOP(codeTop-2)!=END) {
              switch (GETOP(prevInstr)) {
                case TRY_ME_ELSE:	/* begin of single cls */
                  //System.err.println("PREV CLAUSE: TRY_ME_ELSE"); 

                  for (int p=prevInstr-2;p<=codeTop-4;p++)
                    code[p]=code[p+4];

                  SETOP(codeTop-4,END);
                  SETOP(codeTop-2,END);
                  break;

                case RETRY_ME_ELSE:
                  //System.err.println("PREV CLAUSE: TRUST_ME_ELSE"); 
                  SETOP(prevInstr,TRUST_ME_ELSE);
                  break;

                default:
                  dumpCode(codeTop-10,codeTop+2);
                  abortLoad("bad code in backpatching:"+op);
              }
            }
            curPred=pred;
            //System.err.println("CHG: SWITCH:" + codeTop); 
            SETOP(codeTop,SWITCH);
            SETREG(codeTop,0);
            codeTop=SETFUN(codeTop,pred);
            codeTop++;
            //System.err.println("ADD: TRY_ME_ELSE:" + codeTop); 
            SETOP(codeTop,TRY_ME_ELSE);	/* reg = how far is fun=nextcls */
            SETREG(codeTop,arity);	/* arity is here, not on the stack */
          }
          else if (curPred==pred) {
            //System.err.println("CHG: RETRY_ME_ELSE:" + codeTop); 			
            SETOP(codeTop,RETRY_ME_ELSE);
            SETREG(codeTop,arity);
          }
          else {
            //dumpCode(codeTop-5, codeTop+5);
            skipClause=true;
            // $$ abortLoad(
              Interact.warnmes("Predicate "+name+"/"+arity+" leads other group of clauses. Clause ignored!");
            return;
          }
          prevInstr=codeTop;
          codeTop++;
        }
        break;

      case FIRSTARG:			/* MaxReg-FunFirstarg/Arity */
        if (reg>=MAXREG)
          abortLoad("Load instruction: not enough registers"); {
          int funval=inputTerm(name,arity);
          if (name.length()>0&&(name.charAt(0)=='_')||!dict.hdef(curPred,funval,codeTop)) {
            int label=dict.getpred(curPred);
            if (label==prolog.G_empty)
              abortLoad("Load instruction, FIRSTARG: current predicate not found");
            SETOP(label,NONDET);
          }
        }
        codeTop--;				/* null effect, as we do codeTop++ later */
        break;

      case EXECUTE:
        codeTop=SETFUN(codeTop,inputTerm(name,arity));
        break;

      case PUT_VARIABLE:
      case GET_VALUE:
        SETREG(codeTop,reg);
        SETLEFT(codeTop,arity);
        break;

      case GET_STRUCTURE:
      case PUT_STRUCTURE:
        SETREG(codeTop,reg);
        codeTop=SETFUN(codeTop,inputTerm(name,arity));
        break;

      case UNIFY_VARIABLE:
        if (OCOMPRESS(reg,UNIFY_VALUE,UNIFY_VAL_VAR,GET_STRUCTURE,GET_UNIFY_VAL_VAR)) break;
        if (OCOMPRESS(reg,UNIFY_VARIABLE,UNIFY_VAR_VAR,GET_STRUCTURE,GET_UNIFY_VAR_VAR)) break;
        SETREG(codeTop,reg);
        break;

      case UNIFY_VALUE:
        if (OCOMPRESS(reg,UNIFY_VALUE,UNIFY_VAL_VAL,GET_STRUCTURE,GET_UNIFY_VAL_VAL)) break;
        if (OCOMPRESS(reg,UNIFY_VARIABLE,UNIFY_VAR_VAL,GET_STRUCTURE,GET_UNIFY_VAR_VAL)) break;
        SETREG(codeTop,reg);
        break;

      case WRITE_VARIABLE:
        if (OCOMPRESS(reg,WRITE_VALUE,WRITE_VAL_VAR,PUT_STRUCTURE,PUT_WRITE_VAL_VAR)) break;
        if (OCOMPRESS(reg,WRITE_VARIABLE,WRITE_VAR_VAR,PUT_STRUCTURE,PUT_WRITE_VAR_VAR)) break;
        SETREG(codeTop,reg);
        break;

      case WRITE_VALUE:
        if (OCOMPRESS(reg,WRITE_VALUE,WRITE_VAL_VAL,PUT_STRUCTURE,PUT_WRITE_VAL_VAL)) break;
        if (OCOMPRESS(reg,WRITE_VARIABLE,WRITE_VAR_VAL,PUT_STRUCTURE,PUT_WRITE_VAR_VAL)) break;
        SETREG(codeTop,reg);
        break;

      case MOVE_REG:
        if (1==prevLen&&MOVE_REG==GETOP(codeTop-1)&&
            !(1==prevPrevLen&&MOVE_REGx2==GETOP(codeTop-2)))
          SETOP(codeTop-1,MOVE_REGx2);
        SETREG(codeTop,reg);
        SETLEFT(codeTop,arity);
        break;

      case LOAD_VALUE:
        if (1==prevLen&&LOAD_VALUE==GETOP(codeTop-1)
            &&1==GETREG(codeTop-1)&&2==reg) {
          SETOP(codeTop-1,LOAD_VALUEx2);
          SETREG(codeTop-1,arity);
          codeTop--;
          break;
        }
        /* An = REG = second,  Ai = LEFT = first */
        SETREG(codeTop,reg);
        SETLEFT(codeTop,arity);
        break;

      case LOAD_CONSTANT: {
          //System.err.println("LOAD_VAL_SHORT OP:"+op);
          int small;
          if (1==prevLen&&LOAD_VALUE==GETOP(codeTop-1)
            &&1==GETREG(codeTop-1)&&2==reg) {
            small=inputTerm(name,arity);
            int smallVal=OUTPUT_INT(small);
            if (isINTEGER(small)&&smallVal>=0&&smallVal<128) {
              op=LOAD_VAL_SHORT;
              SETOP(codeTop-1,op);
              SETREG(codeTop-1,small);
              codeTop--;
              //System.err.println("LOAD_VAL_SHORT:"+smallVal);
              break;
            }
          }
        }
        SETREG(codeTop,reg);
        codeTop=SETFUN(codeTop,inputTerm(name,arity));
        break;

      case GET_CONSTANT:
      case PUT_CONSTANT:
      //if(prolog.timeStamp==prolog.RUNTIME)
      //  System.err.println("PUT_CONSTANT:"+name+"/"+arity); //- a way to do limited tracing
      case UNIFY_CONSTANT:
      case WRITE_CONSTANT:
        SETREG(codeTop,reg);
        codeTop=SETFUN(codeTop,inputTerm(name,arity));
        break;

      case END:
        skipClause=false; //$$
        SETREG(codeTop,reg);
        codeTop++;
        linkCode(codeTopBak);
        shrink();
        break;

      case ARITH:
        SETOP(codeTop,(GETOP(codeTop)+arity));
        if (reg!=0) {
          SETREG(codeTop,reg);
          SETLEFT(codeTop,name.charAt(0)-'0');
        }
        break;

      case INLINE:
      case BUILTIN:
        SETREG(codeTop,reg);
        SETOP(codeTop,(GETOP(codeTop)+arity));
        break;

      default:
        SETREG(codeTop,reg);
    }
    //if (codeTop >= codeMax) abortLoad("Code store overflow.");
    prevPrevLen=prevLen;
    prevLen=getInstructionLength(op);
    codeTop++;
  }

  private int inputTerm(String name,int arity) throws TypeException,ResourceException {
    return atomTable.inputTerm(name,arity);
  }

  private void linkCode(int instr) throws PrologException {
    //Prolog.dump("linking code starting at: "+instr);

    if (prolog.DEBUG) showLowLevel();

    int label=0;
    int f;
    int instrLen=1;
    for (;instr<codeTop;instr+=instrLen) {
      int op=GETOP(instr);
      //if(op>=MAXOP) {Interact.println(instr+": op:!!!? "+op);instrLen=1;} else 
      instrLen=getInstructionLength(op);
      switch (op) {
        case EXECUTE:			/* we look to what's next */
          f=GETFUN(instr);
          label=prolog.G_empty;
          try {
            label=dict.getpred(f);
          }
          catch (PrologException ignore) {
          } //FIXED - PT

          if (dict.do_isEmpty(label)) {
            if(Interact.verbosity>=2) {
            Interact.warnmes(
                "Undefined after :-, code["+instr+".."+codeTop+"] ->"+NAME(f)+"/"+GETARITY(f));
            }
            
            //abortLoad("Undefined predicate after :-"); //FIXED: this gets caught without message

            //dict.hdef(prolog.G_undefined, f, instr);
            SETOP(instr,APPLY);
            //Interact.println(instr+": label:=>"+GETLABEL(instr));
            continue;
          }
          switch (GETOP(label)) {

            case DEMO_0:
              SETOP(instr,DEMO_0);
              SETREG(instr,1);
              SETOP(instr+getInstructionLength(DEMO_0),NOP);
              SETREG(instr+getInstructionLength(DEMO_0),0);
              break;

            case DEMO_1:
              SETOP(instr,DEMO_1);
              SETREG(instr,1);
              SETOP(instr+getInstructionLength(DEMO_1),NOP);
              SETREG(instr+getInstructionLength(DEMO_1),0);
              break;

            case NONDET:
              SETOP(instr,EXEC_TRY);
              SETLABEL(instr,label+getInstructionLength(NONDET));
              break;

            case TRY_ME_ELSE:
              SETOP(instr,EXEC_TRY);
              SETLABEL(instr,label);
              break;

            case JUMP_IF:
              SETOP(instr,EXEC_JUMP_IF);
              SETLABEL(instr,label);
              break;

            case SWITCH:
              if (linkSwitch(label,false)) {
                SETOP(instr,EXEC_JUMP_IF);
                SETLABEL(instr,label);
              }
              else {
                SETOP(instr,EXEC_SWITCH);
                SETLABEL(instr,label);
              }
              break;

            default:
              SETLABEL(instr,label);
          }
          break; // end EXECUTE

        case SWITCH:
          linkSwitch(instr,true);
          break;

        case NONDET: //if first and last -> TRY_ME_ONLY
          label=dict.getpred(GETFUN(instr));
          //System.err.println("here^^^"+NAME(label)+NAME(prolog.G_empty));
          if (label!=prolog.G_empty) break; //^^^ already linked as a pred, ok as is
          dict.setpred(GETFUN(instr),label+getInstructionLength(NONDET));
          SETOP(label,TRY_ME_ONLY); // because it has just this clause
          break;

        default:
          if (0==instrLen) {
            instrLen=1;
          }
      }
    }
  }

  private boolean linkSwitch(int p,boolean doit) {
    int jlabel=GETLABEL(p+2);
    int label=jlabel;
    if (TRUST_ME_ELSE==GETOP(label)&&
       GET_UNIFY_VAR_VAR==GETOP(label+2)) {
      if (doit) {
        SETOP(p,JUMP_IF);
        label+=2;
        SETLABEL(p,label);
      }
      return jlabel!=0;
    }
    return false;
  }


  /*
  private void abortLoad() throws LoadException {
    abortLoad("Bad code detected. ");
  }
  */
  private void abortLoad(String msg) throws LoadException {
    throw new LoadException("Load instruction: "+msg+" code=@"+code.hashCode()+"top:"+codeTop);
  }

  public void dumpCode(int instrPtr) throws LoadException {
    dumpCode(instrPtr-10,instrPtr+10);
  }

  public void dumpCode(int instrFrom,int instrTo) throws LoadException {
    if (instrFrom<0) instrFrom=0;

    Prolog.dump("CODE: from:"+instrFrom+" to:"+instrTo+" used:"+codeTop+" of:"+code.length+".");

    for (int i=instrFrom;i<instrTo&&i<codeTop;i+=getInstructionLength(GETOP(i)))
      dumpInstruction(i,i);

    Prolog.dump("end of dump");
  }

  public void dumpInstruction(int instrCount,int i) {
    int op=GETOP(i);
    String name=getInstructionName(op);
    switch (op) {
      case END:
        Prolog.dump("#"+instrCount+"\t<"+i+"> op:"+op+"/"+name);
        break;
      case EXECUTE:
      case NONDET:
      case SWITCH:
      case EXEC_SWITCH:
      case JUMP_IF:
      case EXEC_JUMP_IF:
      case EXEC_TRY:
        Prolog.dump("");
      case TRY_ME_ELSE:
      case RETRY_ME_ELSE:
      case TRUST_ME_ELSE:
        Prolog.dump("#"+instrCount+"\t<"+i+"> op:"+op+"/"+name+" "+GETREG(i)+"->["+GETLABEL(i)+"]");
        break;
      case MOVE_REG: 			// MOVE INSTRUCTIONS:
      case PUT_VARIABLE:
      case GET_VALUE:
      case LOAD_VALUE:
      case UNIFY_VAR_VAR:   	// DOUBLE INSTRUCTIONS:
      case WRITE_VAR_VAR:
      case UNIFY_VAL_VAL:
      case WRITE_VAL_VAL:
      case UNIFY_VAR_VAL:
      case WRITE_VAR_VAL:
      case UNIFY_VAL_VAR:
      case WRITE_VAL_VAR:
      case MOVE_REGx2:
      case LOAD_VALUEx2:
      case LOAD_VAL_SHORT:
        Prolog.dump("#"+instrCount+"\t<"+i+"> op:"+op+"/"+name+" X"+GETREG(i)+",A"+GETARG(i));
        break;
      default:
        Prolog.dump("#"+instrCount+"\t<"+i+"> op:"+op+"/"+name);
        if (0!=GETREG(i))
          Prolog.dump(" X"+GETREG(i));
        if (2==getInstructionLength(op)) {
          int f=GETFUN(i);
          //FIXMESystem.err.print(" %s",smartref());
          //if (isINTEGER(f))
          //	System.err.print(" " + OUTPUT_INT(f));
          //else
          Prolog.dump(" "+NAME(f)+"/"+GETARITY(f));
        }
        Prolog.dump("");
    }
  }

  private static final int getInstructionLength(int op) {
    if (op<0||(op>NOP&&op<CLAUSE)||op>=MAXOP)
      Interact.warnmes("warning - bad operation: "+op);
    return instructionLength[op];
  }

  public static byte[] initInstructionLengths() {
    byte[] ilen=new byte[MAXOP];

    // set instruction lengths to 1
    for (int i=0;i<MAXOP;i++) {
      ilen[i]=1;
    }
    // except for:
    ilen[GET_STRUCTURE]=2;
    ilen[PUT_STRUCTURE]=2;
    ilen[UNIFY_CONSTANT]=2;
    ilen[WRITE_CONSTANT]=2;
    ilen[GET_CONSTANT]=2;
    ilen[PUT_CONSTANT]=2;
    ilen[EXECUTE]=2;
    ilen[PROCEED]=2;
    ilen[END]=2;
    ilen[TRY_ME_ELSE]=2;
    ilen[RETRY_ME_ELSE]=2;
    ilen[TRUST_ME_ELSE]=2;
    ilen[TRY_ME_ONLY]=2;
    ilen[NONDET]=2;
    ilen[EXEC_TRY]=2;
    ilen[EXEC_SWITCH]=2;
    ilen[SWITCH]=2;
    ilen[JUMP_IF]=2;
    ilen[EXEC_JUMP_IF]=2;
    ilen[LOAD_CONSTANT]=2;
    ilen[GET_UNIFY_VAR_VAR]=2;
    ilen[GET_UNIFY_VAL_VAL]=2;
    ilen[GET_UNIFY_VAR_VAL]=2;
    ilen[GET_UNIFY_VAL_VAR]=2;
    ilen[PUT_WRITE_VAR_VAR]=2;
    ilen[PUT_WRITE_VAL_VAL]=2;
    ilen[PUT_WRITE_VAR_VAL]=2;
    ilen[PUT_WRITE_VAL_VAR]=2;
    //Interact.println(APPLY+":"+MAXOP);
    //ilen[APPLY] = 2; //$$ not expected to cause problem

    return ilen;
  }

  public static String getInstructionName(int c) {
    return "op_"+c;
  }


  /*
public static String getInstructionName (int c) {
	  switch(c)
	  {
        case GET_STRUCTURE:     return "GET_STRUCTURE";
        case PUT_STRUCTURE:     return "PUT_STRUCTURE";
    
        case UNIFY_VARIABLE:    return "UNIFY_VARIABLE";
        case UNIFY_VALUE:               return "UNIFY_VALUE";
        case UNIFY_CONSTANT:    return "UNIFY_CONSTANT";
    
        case WRITE_VARIABLE:    return "WRITE_VARIABLE";
        case WRITE_VALUE:               return "WRITE_VALUE";
        case WRITE_CONSTANT:    return "WRITE_CONSTANT";
    
        case GET_CONSTANT:      return "GET_CONSTANT";
        case PUT_CONSTANT:      return "PUT_CONSTANT";
    
        case MOVE_REG:          return "MOVE_REG";
        case PUT_VARIABLE:      return "PUT_VARIABLE";
        case GET_VALUE:         return "GET_VALUE";
    
        case EXECUTE:           return "EXECUTE";
        case PROCEED:           return "PROCEED";

        case PUSH_CUT:          return "PUSH_CUT";
        case PUT_CUT:           return "PUT_CUT";
        case GET_CUT:           return "GET_CUT";

        case END:               return "END";
    
        case TRY_ME_ELSE:       return "TRY_ME_ELSE";
        case RETRY_ME_ELSE:     return "RETRY_ME_ELSE";
        case TRUST_ME_ELSE:     return "TRUST_ME_ELSE";
        case TRY_ME_ONLY:       return "TRY_ME_ONLY";

        case NONDET:            return "NONDET";
        case EXEC_TRY:          return "EXEC_TRY";
        case CLAUSE:            return "CLAUSE";
        case SWITCH:            return "SWITCH";
        case EXEC_SWITCH:       return "EXEC_SWITCH";
        case JUMP_IF:           return "JUMP_IF";
        case EXEC_JUMP_IF:      return "EXEC_JUMP_IF";
        case FIRSTARG:          return "FIRSTARG";
        case LOAD_CONSTANT:     return "LOAD_CONSTANT";
        case LOAD_VALUE:        return "LOAD_VALUE";
    
        case UNIFY_VAR_VAR:     return  "UNIFY_VAR_VAR @@";
        case WRITE_VAR_VAR:     return  "WRITE_VAR_VAR @@";

        case UNIFY_VAL_VAL:     return  "UNIFY_VAL_VAL @@";
        case WRITE_VAL_VAL:     return  "WRITE_VAL_VAL @@";

        case UNIFY_VAR_VAL:         return  "UNIFY_VAR_VAL @@";
        case WRITE_VAR_VAL:         return  "WRITE_VAR_VAL @@";

        case UNIFY_VAL_VAR:         return  "UNIFY_VAL_VAR @@";
        case WRITE_VAL_VAR:         return  "WRITE_VAL_VAR @@";
    
        case GET_UNIFY_VAR_VAR:         return  "GET_UNIFY_VAR_VAR @@@";
        case GET_UNIFY_VAL_VAL:         return  "GET_UNIFY_VAL_VAL @@@";
        case GET_UNIFY_VAR_VAL:         return  "GET_UNIFY_VAR_VAL @@@";
        case GET_UNIFY_VAL_VAR:         return  "GET_UNIFY_VAL_VAR @@@";

        case PUT_WRITE_VAR_VAR:         return  "PUT_WRITE_VAR_VAR @@@";
        case PUT_WRITE_VAL_VAL:         return  "PUT_WRITE_VAL_VAL @@@";
        case PUT_WRITE_VAR_VAL:         return  "PUT_WRITE_VAR_VAL @@@";
        case PUT_WRITE_VAL_VAR:         return  "PUT_WRITE_VAL_VAR @@@";

        case MOVE_REGx2:            return  "MOVE_REGx2 @@";
        case LOAD_VALUEx2:          return  "LOAD_VALUEx2 @@";
        case LOAD_VAL_SHORT:        return  "LOAD_VAL_SHORT @@";

        case NOP:                                           return "NOP";

        default:
        if(c>=INLINE && c<=NOP)
          return builtinName[c-INLINE];
        else
          return "*** BAD INSTRUCTION CODE ***";
      }
  }

  private static final String builtinName[] = {
      "FAIL_0",
      "CWRITE_1",
      "NL_0",
      "VAR_1",
      "NONVAR_1",
      "INTEGER_1",
      "ATOMIC_1",
      "IS_COMPILED_1",
      "LIFT_HEAP_0",
      "SEEN_0",
      "TOLD_0",
      "PLUS_3",
      "SUB_3",
      "MUL_3",
      "DIV_3",
      "MOD_3",
      "RANDOM_1",
      "GET0_1",
      "PUT0_1",
      "LESS_2",
      "GREATER_2",
      "LESS_EQ_2",
      "GREATER_EQ_2",
      "ARITH_EQ_2",
      "ARITH_DIF_2",
      "LSHIFT_3",
      "RSHIFT_3",
      "L_AND_3",
      "L_OR_3",
      "L_XOR_3",
      "L_NEG_3",
      "COMPARE0_3",
      "ARG_3",
      "DEF_3",
      "SET_3",
      "VAL_3",
      "RM_2",
      "SYMCAT_3",
      "COPY_TERM_2",
      "SAVE_TERM_2",
      "SEEING_1",
      "TELLING_1",
      "SEE_OR_FAIL_1",
      "TELL_OR_FAIL_1",
      "ADD_INSTR_5",
      "DET_APPEND_3",
      "OUT0_3",
      "RD0_3",
      "IN0_3",
      "EVAL0_3",
      "SREAD_2",
      "SWRITE_2",
      "DEMO_0",
      "DEMO_1",
      "FINDALL_STORE_HEAP_1",
      "FINDALL_LOAD_HEAP_1",
      "FUNCTOR_3",
      "NAME_2",
      "ABORT_0",
      "RESTART_0",
      "SHELL_1",
      "RUNTIME_2",
      "GLOBAL_STACK_2",
      "LOCAL_STACK_2",
      "TRAIL_2",
      "CODE_2",
      "STRINGS_2",
      "SYMBOLS_2",
      "HTABLE_2",
      "LIST_ASM_3",
      "BBOARD_2",
      "BB_LIST_1",
      "BB_RESET_0",
      "PROFILE_0",
      "APPLY",
      "GC_2",
  "LAST_BUILTIN"};
  */

} // end class CodeStore

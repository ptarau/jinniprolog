package prolog.logic;

public class LogicEngine extends HeapStack implements Runnable {
  
  public final static Object call(Object query) {
    LogicEngine E=new LogicEngine();
    return E.query_engine(query);
  }

  public LogicEngine newLogicEngine(Prolog prolog) {
    return new LogicEngine(prolog);
  }

  public LogicEngine(Prolog prolog){
    super(prolog,1024);
    initStacks();
  }

  public LogicEngine(){
    this(Prolog.getDefaultProlog());
  }

  final private void initStacks() {
    this.codeStore=prolog.codeStore;
    this.dict=prolog.dict;
    this.unifyStack=new IntStack(1024);
    this.choice=new ChoicePointStack(this,1);
    this.trail=new TrailStack(this,1,choice);
    // establish otherwise circular references for local ease of use.
    this.choice.setTrail(trail);
    this.regs=new int[TEMPARGS+MAXREG+1];
    this.gc_flag=false;
    this.instance_id=0;
  }

  void stub(String name) {
    Interact
        .fatal_error("### unimplemented instruction called in LogicEngine: "
            +name);
  }

  private CodeStore codeStore;

  private Dict dict;

  private IntStack unifyStack;

  private ChoicePointStack choice;

  private TrailStack trail;

  private ObjectDict undoTable;

  private EncodedTerm messageBox;

  private Object[] ioBundle;
  
  private static final int TEMPARGS=8;

  public int[] regs;

  // curInstr used by run() which need to have global access - to simplify
  // conversion.
  private int curInstr;

  public int instrPtr;

  public int ires;

  /** Work Functor arity */
  private int arity;

  private int cutB; // cut back choice point.

  private int S;

  private boolean gc_flag;

  // ******INTERFACE INSTRUCTION OPERATIONS ********************
  private final int REGFIELD() {
    return LCGET(curInstr);
  }

  private final int LEFTFIELD() {
    return MCGET(curInstr);
  }

  // ******Register manipulation functions *********************
  private final int An() {
    return regs[LCGET(curInstr)];
  }

  private final void setAn(int val) {
    regs[LCGET(curInstr)]=val;
  }

  private final int Ai() {
    return regs[MCGET(curInstr)];
  }

  private final void setAi(int val) {
    regs[MCGET(curInstr)]=val;
  }

  /**
   * Get the value in a temporary register.
   * <p>
   * Tempoary regs use a negative offset from the end of the <tt>regs</tt>
   * array. Propose TEMPREGS = regs[regs.length - i];
   * 
   * @param i
   *          Index of the temporary register in the range 1 to TEMPREGS.
   * 
   */
  public final int X(int i) {
    return regs[regs.length-i];
  }

  /**
   * Set the value of a temporary register.
   * 
   * @return The register contents.
   */
  private final void setX(int i,int val) {
    regs[regs.length-i]=val;
  }

  final private boolean putArg(int i,int x) {
    boolean ok=unify(x,regs[i]);
    if(!ok)
      FAILURE();
    return ok;
  }

  final private void putLastArg(int i,int x) {
    if(!unify(x,regs[i])) { // FREE
      FAILURE();
    } else {
      regs[1]=regs[i+1];
      instrPtr++;
    }
    // expect continue after
  }

  final private void INIT_INTERP(Object query) throws PrologException {
    choice.clear();
    trail.clear();
    clear();
    S=0;
    // regs[1]= // the query !
    regs[2]=newVar(); // added regs[1] to avoid garbage in cp
    // Prolog.dump("r2:"+termToString(regs[2]));
    regs[3]=prolog.G_true;
    instrPtr=prolog.codeStore.getStartPoint();

    regs[1]=queryTerm(query); // this may consume heap, should be last!!!
    // Prolog.dump("getTop()="+getTop()+" r1:"+termToString(regs[1]));
    cutB=choice.addChoicePoint(0,regs,3);
  }

  final private int queryTerm(Object query) throws PrologException {
    if(null==query)
      return newVar(); // var query=>should call toplevel
    // if query!=null => jlib.pl -> run/2
    // Prolog.dump("!!!query:"+query.toString());
    if(query instanceof String)
      return prolog.atomTable.newFunctor((String)query,0);
    else if(query instanceof EncodedTerm)
      return decodedCopy((EncodedTerm)query);
    else {
      termReader.clear();
      int h=termReader.putObject(query);
      return h;
      // return prolog.atomTable.newFunctor(query.toString(), 0);
    }
  }

  private int op;
  
  final private int runInternal() throws PrologException {

    for(;;) {

      curInstr=codeStore.GETINSTR(instrPtr,0);
      op=CodeStore.GETINSTR_OP(curInstr);
      if(Prolog.TRACE&&Prolog.TRACE_ON&&Prolog.instrCount<Prolog.TRACE_STOP) {
        show_trace();
      }
      // prolog.dump("op="+op+" top=="+getTop());
      switch(op) {
      case END:
        // destroy(); - may be reused?
        return 0;

      case UNIFY_VARIABLE:
        if(S!=0) {
          setAn(S++);
          instrPtr++;
          continue;
        }
        // drop thru to next case

      case WRITE_VARIABLE:
        setAn(newVar());
        instrPtr++;
        continue;

      case UNIFY_VALUE:
        if(S!=0) {
          if(!unify(An(),S)) {
            FAILURE();
            continue;
          }
          S++;
          instrPtr++;
          continue;
        }
        // drop thru to next case

      case WRITE_VALUE:
        pushTerm(An());
        instrPtr++;
        continue;

      case GET_STRUCTURE:
        xref=An();
        FDEREF();
        if(isVAR(xval)) {
          S=0;
          pushTerm(codeStore.GETLABEL(instrPtr));
          setRef(xref,getHeapTop());
          trail.trailVarIf(xref);
          instrPtr+=2;
          continue;
        }
        if(xval!=codeStore.GETLABEL(instrPtr)) {
          FAILURE();
          continue;
        }
        S=xref+1;
        instrPtr+=2;
        continue;

      case PUT_STRUCTURE:
        setAn(pushTerm(codeStore.GETLABEL(instrPtr)));
        instrPtr+=2;
        continue;

      case MOVE_REG:
        setAn(Ai());
        instrPtr++;
        continue;

      case MOVE_REGx2:
        setAn(Ai());
        curInstr=codeStore.GETINSTR(instrPtr,1);
        setAn(Ai());
        instrPtr+=2;
        continue;

      case PUT_VARIABLE:
        setAn(newVar());
        setAi(An());
        instrPtr++;
        continue;

      case GET_VALUE:
        if(!unify(An(),Ai())) {
          FAILURE();
          continue;
        }
        instrPtr++;
        continue;

      case UNIFY_VAR_VAR:
        if(S!=0) {
          setAn(S++);
          setAi(S++);
          instrPtr++;
          continue;
        }
      case WRITE_VAR_VAR:
        setAn(newVar());
        setAi(newVar());
        instrPtr++;
        continue;

      case UNIFY_VAL_VAR:
        if(S!=0) {
          if(!unify(An(),S)) {
            FAILURE();
            continue;
          }
          S++;
          setAi(S++);
          instrPtr++;
          continue;
        }

      case WRITE_VAL_VAR:
        pushTerm(An());
        setAi(newVar());
        instrPtr++;
        continue;

      case UNIFY_VAL_VAL:
        if(S!=0) {
          if(!unify(An(),S)) {
            FAILURE();
            continue;
          }
          S++;
          if(!unify(Ai(),S)) {
            FAILURE();
            continue;
          }
          S++;
          instrPtr++;
          continue;
        }

      case WRITE_VAL_VAL:
        pushTerm(An());
        pushTerm(Ai());
        instrPtr++;
        continue;

      case UNIFY_VAR_VAL:
        if(S!=0) {
          setAn(S++);
          if(!unify(Ai(),S)) {
            FAILURE();
            continue;
          }
          S++;
          instrPtr++;
          continue;
        }

      case WRITE_VAR_VAL:
        setAn(newVar());
        pushTerm(Ai());
        instrPtr++;
        continue;

      case UNIFY_CONSTANT:
        if(S!=0) {
          xval=getRef(S++);
          xref=xval;
          FDEREF();

          if(isVAR(xval)) {
            setRef(xref,codeStore.GETLABEL(instrPtr));
            trail.trailVarIf(xref);
          } else if(xval!=codeStore.GETLABEL(instrPtr)) {
            FAILURE();
            continue;
          }
          instrPtr+=2;
          continue;
        }
        // drop thru to WRITE_CONSTANT.

      case WRITE_CONSTANT:
        pushTerm(codeStore.GETLABEL(instrPtr));
        instrPtr+=2;
        continue;

      case GET_CONSTANT:
        // Bind VAR in An to constant.
        xref=An();
        FDEREF();

        if(isVAR(xval)) {
          setRef(xref,codeStore.GETLABEL(instrPtr)); // fill the unb. VAR with
                                                      // the const.
          trail.trailVarIf(xref);
        } else if(xval!=codeStore.GETLABEL(instrPtr)) { // if already bound fail
                                                        // if not same value.
          FAILURE();
          continue;
        }
        instrPtr+=2;
        continue;

      case PUT_CONSTANT:
        setAn(codeStore.GETLABEL(instrPtr));
        instrPtr+=2;
        continue;

      case PUSH_CUT:
        pushTerm(INPUT_INT(cutB));
        instrPtr++;
        continue;

      case PUT_CUT:
        choice.setCut(cutB);
        instrPtr++;
        continue;

      case GET_CUT:
        xref=regs[1];
        FDEREF();
        choice.setCut(OUTPUT_INT(xval));
        instrPtr++;
        continue;

      case EXECUTE:
        instrPtr=codeStore.GETLABEL(instrPtr);
        if(gc_flag&&!gc_call())
          return 0;
        cutB=choice.getTop();
        continue;

      case PROCEED:
        FAILURE(); // FAILURE() is ok, HERE !!!
        // it just sets the address of next instruction
        // heap address where the answer is found
        // alternatively, 0 means no answer has been computed
        return 1; // address of the answer

      case EXEC_JUMP_IF:
        instrPtr=codeStore.GETLABEL(instrPtr);
        if(gc_flag&&!gc_call())
          return 0;
        cutB=choice.getTop();
      case JUMP_IF:
        xref=regs[1];
        FDEREF();
        // System.err.println("JUMP IF:" + xref + "/" + xval);
        if(isVAR(xval)) {
          instrPtr+=2;
          continue;
        }
        {
          int label=codeStore.GETLABEL(instrPtr);
          // System.err.println("JUMP IF:" + label
          // +"?"+codeStore.GETLABEL(label) );
          if(xval!=codeStore.GETLABEL(label)) {
            instrPtr+=4;
            continue;
          }
          S=xref+1;
          curInstr=codeStore.GETINSTR(label,2);
          // System.err.println("\tAn: R" + LCGET(curInstr) + "="+ An() + ", Ai:
          // R" + MCGET(curInstr) + "=" + Ai());
          setAn(S++);
          setAi(S++);
          instrPtr=label+3;
          continue;
        }

      case GET_UNIFY_VAL_VAR:
        xref=An();
        FDEREF();

        if(isVAR(xval)) {
          S=0;
          setRef(xref,pushTerm(codeStore.GETLABEL(instrPtr)));
          trail.trailVarIf(xref);
          curInstr=codeStore.GETINSTR(instrPtr,2);
          pushTerm(An());
          setAi(newVar());
          instrPtr+=3;
          continue;
        }
        if(xval!=codeStore.GETLABEL(instrPtr)) {
          FAILURE();
          continue;
        }
        S=xref+1;
        curInstr=codeStore.GETINSTR(instrPtr,2);
        if(!unify(An(),S)) {
          FAILURE();
          continue;
        }
        S++;
        setAi(S++);
        instrPtr+=3;
        continue;

      case GET_UNIFY_VAL_VAL:
        xref=An();
        FDEREF();
        if(isVAR(xval)) {
          S=0;
          setRef(xref,pushTerm(codeStore.GETLABEL(instrPtr)));
          trail.trailVarIf(xref);
          curInstr=codeStore.GETINSTR(instrPtr,2);
          pushTerm(An());
          pushTerm(Ai());
          instrPtr+=3;
          continue;
        }
        if(xval!=codeStore.GETLABEL(instrPtr)) {
          FAILURE();
          continue;
        }
        S=xref+1;
        curInstr=codeStore.GETINSTR(instrPtr,2);
        if(!unify(An(),S)) {
          FAILURE();
          continue;
        }
        S++;
        if(!unify(Ai(),S)) {
          FAILURE();
          continue;
        }
        S++;
        instrPtr+=3;
        continue;

      case GET_UNIFY_VAR_VAR:
        xref=An();
        FDEREF();
        if(isVAR(xval)) {
          S=0;
          setRef(xref,pushTerm(codeStore.GETLABEL(instrPtr)));
          trail.trailVarIf(xref);
          curInstr=codeStore.GETINSTR(instrPtr,2);
          setAn(newVar());
          setAi(newVar());
          instrPtr+=3;
          continue;
        }
        if(xval!=codeStore.GETLABEL(instrPtr)) {
          FAILURE();
          continue;
        }
        S=xref+1;
        curInstr=codeStore.GETINSTR(instrPtr,2);
        setAn(S++);
        setAi(S++);
        instrPtr+=3;
        continue;

      case GET_UNIFY_VAR_VAL:
        xref=An();
        FDEREF();
        if(isVAR(xval)) {
          S=0;
          setRef(xref,pushTerm(codeStore.GETLABEL(instrPtr)));
          trail.trailVarIf(xref);
          curInstr=codeStore.GETINSTR(instrPtr,2);
          setAn(newVar());
          pushTerm(Ai());
          instrPtr+=3;
          continue;
        }
        if(xval!=codeStore.GETLABEL(instrPtr)) {
          FAILURE();
          continue;
        }
        S=xref+1;
        curInstr=codeStore.GETINSTR(instrPtr,2);
        setAn(S++);
        if(!unify(Ai(),S)) {
          FAILURE();
          continue;
        }
        S++;
        instrPtr+=3;
        continue;

      case PUT_WRITE_VAR_VAR:
        setAn(pushTerm(codeStore.GETLABEL(instrPtr)));
        instrPtr+=2;
        curInstr=codeStore.GETINSTR(instrPtr,0);
        setAn(newVar());
        setAi(newVar());
        instrPtr++;
        continue;

      case PUT_WRITE_VAL_VAR:
        setAn(pushTerm(codeStore.GETLABEL(instrPtr)));
        instrPtr+=2;
        curInstr=codeStore.GETINSTR(instrPtr,0);
        pushTerm(An());
        setAi(newVar());
        instrPtr++;
        continue;

      case PUT_WRITE_VAL_VAL:
        setAn(pushTerm(codeStore.GETLABEL(instrPtr)));
        instrPtr+=2;
        curInstr=codeStore.GETINSTR(instrPtr,0);
        pushTerm(An());
        pushTerm(Ai());
        instrPtr++;
        continue;

      case PUT_WRITE_VAR_VAL:
        setAn(pushTerm(codeStore.GETLABEL(instrPtr)));
        instrPtr+=2;
        curInstr=codeStore.GETINSTR(instrPtr,0);
        setAn(newVar());
        pushTerm(Ai());
        instrPtr++;
        continue;

      case EXEC_TRY:
        instrPtr=codeStore.GETLABEL(instrPtr);
        cutB=choice.getTop();
        curInstr=codeStore.GETINSTR(instrPtr,0);
      case TRY_ME_ELSE:
        arity=REGFIELD();
        if(gc_flag&&!gc_call(arity))
          return 0; // moved here - as we know arity
        choice.addChoicePoint(codeStore.GETLABEL(instrPtr),regs,arity);
        instrPtr+=2;
        continue;

      case RETRY_ME_ELSE:
        // arity=REGFIELD(); // arity not required as known by choicePoint.
        // backpatch choice so that restoreState will then set instrPtr.
        choice.setSAVED_P(codeStore.GETLABEL(instrPtr));
        cutB=choice.restoreState(regs,false);
        instrPtr+=2;
        continue;

      case TRUST_ME_ELSE:
        // xval=REGFIELD(); // arity not required as known by choicePoint.
        cutB=choice.restoreState(regs,true);
        instrPtr+=2;
        continue;

      case TRY_ME_ONLY: /* nop */
      case NONDET:
        instrPtr+=2;
        continue;

      case EXEC_SWITCH:
        instrPtr=codeStore.GETLABEL(instrPtr);
        if(gc_flag&&!gc_call())
          return 0;
        cutB=choice.getTop();
      case SWITCH:
        xref=regs[1];
        FDEREF();
        if(isVAR(xval)) {
          instrPtr+=2;
          continue;
        }
        instrPtr=dict.hget(codeStore.GETLABEL(instrPtr),xval);
        if(dict.do_isEmpty(instrPtr)) {
          FAILURE();
          continue;
        }
        S=xref+1;
        instrPtr+=2;
        continue;

      case LOAD_CONSTANT:
        xval=REGFIELD();
        setX(xval,codeStore.GETLABEL(instrPtr));
        instrPtr+=2;
        continue;

      case LOAD_VAL_SHORT:
        // REG contains small integer for immediate use.
        xval=INPUT_INT(REGFIELD());
        // was different in orig C, code - but it looks ok here as is PT
        // xval=REGFIELD(); // was in orig C code
        setX(2,xval);
        IN(1,Ai());
        instrPtr++;
        continue;

      case LOAD_VALUEx2:
        IN(1,Ai());
        IN(2,An());
        instrPtr++;
        continue;

      case LOAD_VALUE:
        ires=REGFIELD();
        IN(ires,Ai());
        instrPtr++;
        continue;

        /*
         * BUG: LOAD_VARIABLE: missing - problem with voids passed to
         * arithbuiltins
         */

        /* INLINE_PREDS */

      case FAIL_0:
        FAILURE();
        continue;

      case CWRITE_1:
      case NL_0:
        write_nl_io(op);
        continue;

      case VAR_1:
        xref=regs[0];
        FDEREF();
        if(isNONVAR(xval))
          FAILURE();
        else
          instrPtr++;
        continue;

      case NONVAR_1:
        xref=regs[0];
        FDEREF();
        if(isVAR(xval))
          FAILURE();
        else
          instrPtr++;
        continue;

      case INTEGER_1:
        xref=regs[0];
        FDEREF();
        if(isINTEGER(xval))
          instrPtr++;
        else
          FAILURE();
        continue;

      case ATOMIC_1:
        xref=regs[0];
        FDEREF();
        if(isATOMIC(xval))
          instrPtr++;
        else
          FAILURE();
        continue;

      case IS_COMPILED_1:
        xref=regs[0];
        FDEREF();
        xval=PUTARITY(xval,1+GETARITY(xval));
        if(isCompiled(xval))
          instrPtr++;
        else
          FAILURE();
        continue;

      case RETURN_1:
        xref=regs[0]; // FDEREF();
        // FAILURE();
        instrPtr++;
        return xref;

      case SEEN_0:
      case TOLD_0:
        seen_told_io(op);
        continue;

        /* INLINE ARITH */

      case PLUS_3:
      case SUB_3:
      case MUL_3:
      case DIV_3:
      case MOD_3:
      case LSHIFT_3:
      case RSHIFT_3:
      case L_AND_3:
      case L_OR_3:
      case L_XOR_3:
        COMPUTE(op);
        continue;

      case RANDOM_1: {
        OUT(INPUT_INT(prolog.getRandom()));
        continue;
      }

      case GET0_1:
      case PUT0_1:
        byte_io(op);
        continue;

        // Relational operators.
      case LESS_2:
      case GREATER_2:
      case LESS_EQ_2:
      case GREATER_EQ_2:
      case ARITH_EQ_2:
      case ARITH_DIF_2:
        MUST_BE(op);
        continue;

      case L_NEG_3: // not a bitset difference !!! first arg should always be 0
        OUT(INPUT_INT(X(1))|(INPUT_INT(~OUTPUT_INT(X(2)))));
        continue;

      case COMPARE0_3:
        ires=compare(X(1),X(2));
        xval=prolog.compare_vals[ires+1];
        OUT(xval);
        continue;

      case ARG_3:
        arg();
        continue;

      case DEF_3:
      case SET_3:
      case VAL_3:
      case RM_2:
        bb_op(op);
        continue;

      case CREATE_ENGINE_3:
      case ENGINE_GET_2:
      case ENGINE_STOP_1:
        engine_op(op);
        continue;

      case NEW_FLUENT_3:
        new_fluent();
        continue;

      case NAMECAT_4:
        xval=namecat(X(1),X(2),X(3));
        if(xval==0) {
          FAILURE();
          continue;
        }
        OUT(xval);
        continue;

      case COPY_TERM_2:
        xval=X(1);
        if(!isATOMIC(xval)) {
          // this extra test is just an optimization - avoids some
          // heavy initialization inside copy Term
          xval=copyTerm(xval);
          if(0==xval) {
            FAILURE();
            continue;
          }
        }
        OUT(xval);
        continue;

      case NEW_CODE_2: {
        xval=X(1);
        if(!isSYMCONST(xval))
          throw new ExistenceException("bad class name: "+termToString(xval));
        // could be this prolog - as it will be rolled back - but why?
        // delegation is dynamic anyway
        Prolog other=Prolog.cloneOrigProlog(getAtomName(xval));
        // we unroll the clone of the default Prolog - as it might have been
        // used to load files - with unexpected conflicts
        // xval=INPUT_INT(ires);
        xval=termReader.putObject(other);

        OUT(xval);
        continue;
      }

      case SREAD0_4: // $IO
      case SWRITE_2: // $IO
        string_io(op);
        continue;

      case NEW_JAVA_CLASS_2:
      case NEW_JAVA_OBJECT_3:
      case INVOKE_JAVA_METHOD_5:
      case DELETE_JAVA_CLASS_2:
      case DELETE_JAVA_OBJECT_2:
      case GET_JAVA_FIELD_HANDLE_3:
        refl_op(op);
        continue;

      case RUN_BG_2: // !!
        bg();
        continue;

      case CURRENT_ENGINE_1:
        // xval=termReader.putObject(this);
        ires=addObject(this);
        xval=INPUT_INT(ires);
        OUT(xval);
        continue;

        /* sends an encoded term to an engine */
      case TO_ENGINE_2:
        to_engine(X(1),X(2));
        instrPtr++;
        continue;

        /* decodes to fresh heap term the encoded term sent to this engine */
      case FROM_ENGINE_1:
        xval=from_engine();
        OUT(xval);
        continue;

        /*
         * to replace these with actual builtins edit also builtins.pl and
         * BuiltinIDs.java to be used only for preformance critical builtins -
         * use Reflection interface otherwise
         */
      case JCALL_3: {
        xval=jcall(X(1),X(2));
        // xval=prolog.atomTable.newFunctor(y,0);
        OUT(xval);
      }
        continue;

      case QUEUE_CREATE_1:
      case QUEUE_SIZE_2:
      case QUEUE_OP_4:
      case QUEUE_DESTROY_1:
      case QUEUE_ADD_2:
      case QUEUE_POP_2:
      case QUEUE_PUSH_2:
      case QUEUE_UPDATE_AT_3:
      case QUEUE_LIST_2:
        queue_op(op);
        continue;

      case CHANGE_ARG_3:
        change_arg();
        instrPtr++;
        continue;

      case SEEING_1:
      case TELLING_1:
      case SEE_OR_FAIL_1:
      case TELL_OR_FAIL_1:
        file_io(op);
        continue;

      case ADD_INSTR_5: // $IO
        // uses: X(1):0=mem,1=wam, X(2)=op, X(3)=reg, X(4)=name, X(5)=arity.
        add_instr();
        instrPtr++;
        continue;

      case DET_APPEND_3:
        xref=det_append(); // uses X(1) and X(2) implicitely.
        if(xref==0) {
          FAILURE();
          continue;
        }
        OUT(xref);
        continue;

        /* OLD SPECIAL BUILTINS */

        /*
         * handles a. which gives a(Cont):-demo(Cont).
         */
      case DEMO_0:
        cutB=choice.getTop();
        xref=An();

        FDEREF();
        arity=GETARITY(xval);
        instrPtr=dict.getpred(xval);

        if(dict.do_isEmpty(instrPtr)) {
          // int cont=regs[arity+1];
          int cont=getRef(xref+arity); // BUG - past versions was cont=arity or
                                        // arity+1
          instrPtr=do_undef(0,xref,cont);
          continue;
        }

        for(int i=1;i<arity+1;i++)
          regs[i]=getRef(xref+i); // was gc bug

        if(gc_flag&&!gc_call(arity))
          return 0;

        continue;

        /*
         * handles pred like: a:-X,b. which become a(Cont):-demo(X,b(Cont)).
         */
      case DEMO_1:
        cutB=choice.getTop();
        xref=An();
        FDEREF();
        arity=GETARITY(xval)+1; // inc arity by 1 for bin continution
        instrPtr=dict.getpred(PUTARITY(xval,arity));

        if(dict.do_isEmpty(instrPtr)) {
          int cont=regs[2];
          instrPtr=do_undef(1,xref,cont);
          continue;
        }

        regs[arity]=regs[2];
        for(int i=1;i<arity;i++) {
          regs[i]=getRef(xref+i);
        }

        if(gc_flag&&!gc_call(arity))
          return 0;
        continue;

      case CLASS_NAME_1: {
        String s=prolog.className;
        // if(null==s) s="prolog";
        xval=prolog.atomTable.newFunctor(s,0);
      }

        if(!unify(xval,regs[1])) {
          FAILURE();
          continue;
        }
        regs[1]=regs[2];

        instrPtr++;
        continue;

      case THIS_CLASS_1:
        ires=prolog.atomTable.o2i(prolog);
        xval=INPUT_INT(ires);
        if(!unify(xval,regs[1])) {
          FAILURE();
          continue;
        }
        regs[1]=regs[2];

        instrPtr++;
        continue;

      case FUNCTOR_3:
        if(!functor(regs[1],regs[2],regs[3])) {
          FAILURE();
          continue;
        }
        regs[1]=regs[4];
        instrPtr++;
        continue;

      case NAME_3:
        if(!name2list(regs[1],regs[2],regs[3])) {
          FAILURE();
          continue;
        }
        regs[1]=regs[4];
        instrPtr++;
        continue;

      case ABORT_0:
        return 0;

      case RESTART_0: /* can be used only from system zone */
        prolog.hard_rollback();
        instrPtr++;
        continue;

      case SHELL_1: // $IO
        shell();
        continue;

      case RUNTIME_2:
      case GLOBAL_STACK_2:
      case LOCAL_STACK_2:
      case TRAIL_2:
      case CODE_2:
      case STRINGS_2:
      case SYMBOLS_2:
      case HTABLE_2:
        if(!stats(op))
          FAILURE();
        continue;

      case LIST_ASM_3:
        list_asm(regs);
        regs[1]=regs[4];
        instrPtr++;
        continue;

      case SERIALIZE_1:
        xref=regs[1];
        regs[1]=regs[2];
        instrPtr++;
        serialize(xref);
        continue;

      case BB_LIST_1:
        throw new ExistenceException("Not implemented: BB_LIST_1");

      case STOP_0:
        stop();
        return 0;

      case PROFILE_0:
        if(prolog.DEBUG)
          trace_on(); // System.err.println(""+atomTable.marks);
        Prolog.dump("dict:");
        dict.dump();
        instrPtr++;
        continue;

      case GC_2: {
        xref=regs[1];
        putLastArg(2,INPUT_INT(getFree()));
        gc_flag=true;
      }
        continue;

      case APPLY:
        cutB=choice.getTop();
        xval=codeStore.GETLABEL(instrPtr);
        apply(xval);
        instrPtr=dict.getpred(prolog.G_metacall);
        continue;

      default:
        warn_mes("*** bad instruction: ["+op+"] ***");
        stop();
        return 0;

      } // end switch
    } // end for
  }

  protected void expand() {
    gc_flag=true;
    super.expand();
  }

  void gc_trace(String mes,int pred,int arity) throws ExistenceException {
    if(PrologGC.trace>1) {
      String data=(pred!=0)?(getAtomName(pred)+"/"+arity):("<?>/"+arity);
      Prolog.dump("GC: "+data+" "+mes+" USED: "+getUsed()+" FREE:"+getFree());
    }
  }

  /**
   * calls external GC - passes a handle to this LogicEngine to it
   */
  private boolean gc(int pred,int arity) throws PrologException {
    /**
     * call your gc here !!!
     */
    PrologGC collector=new PrologGC(arity,regs,choice,trail,(HeapStack)this);
    boolean ok=collector.collect();
    collector=null;
    gc_flag=false;
    return ok;
  }

  private boolean gc_call(int arity) throws PrologException {
    gc_trace("BEFORE GC call:",0,arity);
    boolean ok=gc(0,arity);
    gc_trace("AFTER GC call: ",0,arity);
    return ok;
  }

  private boolean gc_call() throws PrologException {
    int pred=dict.addr2fun(instrPtr);
    if(dict.do_isEmpty(pred)&&PrologGC.trace>1) {

      Prolog
          .dump("address does not match known predicate: dropped gc_call at: "
              +instrPtr);
      return true; // we cannot compute arity - wait for next opportunity for
                    // gc
    }
    int arity=GETARITY(pred);
    gc_trace("BEFORE:",pred,arity);
    boolean ok=gc(pred,arity);
    gc_trace("AFTER: ",pred,arity);
    return ok;
  }

  final void apply(int fun) {
    int arity=GETARITY(fun)-1;
    fun=PUTARITY(fun,arity);
    pushTerm(fun);
    int xref=getHeapTop();
    // COPY_CELLS(xref,regs,arity);
    pushCells(regs,1,arity);
    // for(int i=1; i<=arity; i++) pushTerm(regs[i]);
    regs[2]=regs[arity+1];
    regs[1]=xref;
    // Prolog.dump("APPLY: "+termToString(xref));
  }

  final boolean isCompiled(int PredArity) throws PrologException {
    int instrPtr=dict.getpred(PredArity);
    return !dict.do_isEmpty(instrPtr);
  }

  final int do_undef(int trim,int closure,int cont) throws PrologException {
    regs[1]=INPUT_INT(trim);
    regs[2]=closure;
    regs[3]=cont;
    int undef=prolog.G_undefined;
    int ip=dict.getpred(undef);
    if(dict.do_isEmpty(ip))
      throw new ExistenceException("undefined predicate: "
          +termToString(closure));
    // Prolog.dump("do_undef: "+trim+" xref: "+
    // termToString(closure)+" cont:"+termToString(cont)+" ip: "+ip);
    return ip;
  }

  final boolean unify(int v1,int v2) {
    unifyStack.clear();
    unifyStack.push(v1);
    unifyStack.push(v2);

    while(!unifyStack.isEmpty()) {
      int t1=unifyStack.pop();
      int t2=unifyStack.pop();
      if(isVAR(t1)) {
        deref(t1);
        t1=xref;
        v1=xval;
      } else
        v1=t1;
      if(isVAR(t2)) {
        deref(t2);
        t2=xref;
        v2=xval;
      } else
        v2=t2;
      if(t1!=t2) {
        if(isVAR(v1)) { /* unb. var. v1 */
          if(isVAR(v2)&&v2>v1) { /* unb. var. v2 */
            setRef(v2,v1);
            trail.trailVarIf(v2);
          } else {
            setRef(v1,isCOMPOUND(v2)?t2:v2);
            trail.trailVarIf(v1);
          }
        } else if(isVAR(v2)) { /* v1 is NONVAR */
          setRef(v2,isCOMPOUND(v1)?t1:v1);
          trail.trailVarIf(v2);
        } else if(v1!=v2) /* both are NONVAR */
          return false;
        else if(isIDENTIFIER(v1)&&((v1=GETARITY(v1))>0)) {
          /* they have the same FUNCTOR, v1==v2 */
          // CHECK(U,ChoiceStk,"unification overflow");
          unifyStack.push(getRef(t1+v1));
          unifyStack.push(getRef(t2+v1));
          for(int i=0;i<v1;i++) {
            unifyStack.push(getRef(t1+i));
            unifyStack.push(getRef(t2+i));
          }
        } // end else if
      } // end t1!=t2
    }
    return true;
  }

  /* ERROR HANDLERS */
  public static void warn_mes(String mes) {
    Interact.warnmes(Interact.NL+"Warning: "+mes+".");
  }

  public final void OUT(int expr) {
    if(LEFTFIELD()!=0) {
      if(!unify(expr,An())) {
        FAILURE();
        return;
      }
    } else {
      setAn(expr);
    }
    instrPtr++;
  }

  private final int float_op(int op,int x,int y) throws TypeException {
    double fx;
    if(isINTEGER(x))
      fx=(double)OUTPUT_INT(x);
    else
      fx=refToDouble(x);

    double fy;
    if(isINTEGER(y))
      fy=(double)OUTPUT_INT(y);
    else
      fy=refToDouble(y);

    double result;
    switch(op) {
    case PLUS_3:
      result=fx+fy;
      break;
    case SUB_3:
      result=fx-fy;
      ;
      break;
    case MUL_3:
      result=fx*fy;
      break;
    case DIV_3:
      result=fx/fy;
      break;
    default:
      throw new TypeException("bad data in arithmetic operation: "
          +CodeStore.getInstructionName(op)+"("+termToString(x)+","
          +termToString(y)+")");
    }
    int iresult=(int)result;
    if(result==(double)iresult)
      return INPUT_INT(iresult);
    else
      return termReader.putFloat(result);
  }

  private final void COMPUTE(int operator) throws TypeException,
      ExistenceException {
    int i1=X(1);
    int i2=X(2);
    if(!isINTEGER(i1)||!isINTEGER(i2)) {
      OUT(float_op(operator,i1,i2));
      return;
    }
    int ires;
    i1=OUTPUT_INT(i1);
    i2=OUTPUT_INT(i2);

    switch(operator) {
    case PLUS_3:
      ires=i1+i2;
      break;
    case SUB_3:
      ires=i1-i2;
      break;
    case MUL_3:
      ires=i1*i2;
      break;
    case DIV_3:
      if(i2==0) {
        warn_mes("Division by zero.");
        FAILURE();
        return;
      }
      ires=i1/i2;
      break;
    case MOD_3:
      ires=i1%i2;
      break;
    case LSHIFT_3:
      ires=i1<<i2;
      break;
    case RSHIFT_3:
      ires=i1>>i2;
      break;
    case L_AND_3:
      ires=i1&i2;
      break;
    case L_OR_3:
      ires=i1|i2;
      break;
    case L_XOR_3:
      ires=i1^i2;
      break;
    default:
      throw new ExistenceException("Undefined arithmetic operation: "+operator);
    }
    OUT(INPUT_INT(ires));
  }

  private final void MUST_BE(int relop) throws TypeException {
    ires=X(1);
    xval=X(2);

    if(!isINTEGER(ires)||!isINTEGER(xval)) {
      if(float_rel(relop,ires,xval))
        instrPtr++;
      else
        FAILURE();
      return;
    }

    int i1=OUTPUT_INT(ires);
    int i2=OUTPUT_INT(xval);
    switch(relop) {
    case LESS_2:
      if(i1>=i2) {
        FAILURE();
        return;
      }
      break;
    case GREATER_2:
      if(i1<=i2) {
        FAILURE();
        return;
      }
      break;
    case LESS_EQ_2:
      if(i1>i2) {
        FAILURE();
        return;
      }
      break;
    case GREATER_EQ_2:
      if(i1<i2) {
        FAILURE();
        return;
      }
      break;
    case ARITH_EQ_2:
      if(i1!=i2) {
        FAILURE();
        return;
      }
      break;
    case ARITH_DIF_2:
      if(i1==i2) {
        FAILURE();
        return;
      }
      break;
    default:
      throw new TypeException("Undefined relational operator: "+relop);
    }
    instrPtr++;
  }

  public final void FAILURE() {
    instrPtr=choice.SAVED_P();
  }

  private final void IN(int I,int Expr) {
    xref=Expr;
    if(isNONVAR(xref))
      setX(I,xref);
    else {
      deref();

      setX(I,isCOMPOUND(xval)?xref:xval);

      if(Prolog.DEBUG) {
        if(prolog.timeStamp==prolog.RUNTIME&&isVAR(X(I))&&isINTEGER(xval)
            &&isCOMPOUND(xval))
          Prolog.dump("IN:"+termToString(X(I))+" xval:"+xval);
      }
    }
  } // End IN

  void list_asm(int regs[]) throws PrologException {
    xref=regs[1];
    FDEREF();
    xval=PUTARITY(xval,1+GETARITY(xval));
    int adr=dict.getpred(xval);
    Prolog.dump("pred:"+adr+" "+termToString(xref));
    codeStore.dumpCode(adr,adr+20);
  }

  private final String toCanonical(int x) {
    String s=null;
    if(isVAR(x))
      x=getRef(x);
    if(isIDENTIFIER(x)) {
      s=getAtomName(x);
      int n=GETARITY(x);
      if(n>0)
        s=s+"z_z"+n;
    } else if(isINTEGER(x))
      s=""+OUTPUT_INT(x);
    else
      s="v_v"+x;
    return s;
  }

  private int namecat(int x1,int x2,int x3) throws PrologException {
    String sbuf=toCanonical(x1)+toCanonical(x2)+toCanonical(x3);

    return prolog.atomTable.newFunctor(sbuf,0);
  }

  private final void add_instr() throws PrologException {
    String name;
    int xval=X(4);
    if(isINTEGER(xval))
      name=""+OUTPUT_INT(xval);
    else if(isIDENTIFIER(xval)) {
      name=getAtomName(xval);
    } else
      throw new LoadException("unexpected instruction <name> field: "+xval);

    add_instr(OUTPUT_INT(X(1)),OUTPUT_INT(X(2)),OUTPUT_INT(X(3)),name,
        OUTPUT_INT(X(5)));
  }

  public final void add_instr(int target,int op,int reg,String fun,int arity)
      throws PrologException {
    if(op<0||op>MAXOP||null==fun)
      throw new LoadException("unexpected in add_instr("+target+","+op+","+reg
          +","+fun+","+arity+")");
    if(0==target)
      codeStore.loadInstruction(op,reg,fun,arity);
    else
      write_instr(target,op,reg,fun,arity);
  }

  public void write_instr(int target,int op,int reg,String fun,int arity)
      throws PrologException {
    stub("write_instr");
  }

  private int det_append() throws TypeException {
    int H=getHeapTop()+1;
    xref=X(1);
    if(isVAR(xref)) {
      xval=getRef(xref);
      while(xval==prolog.G_DOT) {
        pushList(++xref);
        ++xref;
        FDEREF();
      }
    } else
      xval=xref;

    if(xval!=prolog.G_NIL)
      throw new TypeException("[] should end 1-st arg of det_append/3, not "
          +xval);
    /*
     * bug here - maybe unify does not know obout this!
     */
    pushTerm(X(2));
    return H;
  }

  private boolean functor(int t,int f,int n) throws TypeException {
    int r,i,arity;
    if(isVAR(t)) {
      deref(t);
      t=xref;
      r=xval;
    } else
      r=t;
    arity=GETARITY(r);
    if(isNONVAR(r)) {
      if(isATOMIC(r)) {
        if(!unify(f,r))
          return false;
        if(!unify(n,INPUT_INT(0)))
          return false;
      } else {
        if(!unify(f,PUTARITY(r,0)))
          return false;
        if(!unify(n,INPUT_INT(arity)))
          return false;
      }
    } else { /* t is a variable */
      if(isVAR(f)) {
        deref(f);
        f=xref;
        r=xval;
      } else
        r=f;
      if(isVAR(n)) {
        deref(n);
        n=xref;
        i=xval;
      } else
        i=n;
      if(!isINTEGER(i))
        return false;
      arity=OUTPUT_INT(i);
      if(isINTEGER(r)) {
        if(arity!=0)
          return false;
        if(!unify(t,r))
          return false;
      } else if(isNONVAR(r)) {
        int s=getHeapTop()+1;
        if(arity>=MAXARITY)
          throw new TypeException("Error: functor has bad arity:"+dumpCell(r));
        pushTerm(PUTARITY(r,arity));
        while((arity--)>0) {
          newVar();
        }
        if(!unify(t,s))
          return false;
      } else
        return false;
    }
    return true;
  }

  private boolean name2list(int t,int l,int iconvert) throws PrologException {
    if(isVAR(iconvert)) {
      deref(iconvert);
      iconvert=xval;
    }
    // boolean convert=(prolog.G_true==iconvert)||(prolog.G_null==iconvert);
    int vt,vl;
    if(isVAR(t)) {
      deref(t);
      t=xref;
      vt=xval;
    } else
      vt=t;
    if(isVAR(l)) {
      deref(l);
      l=xref;
      vl=xval;
    } else
      vl=l;

    String sbuf;
    if(isNONVAR(vl)&&((sbuf=list2buf(l,vl))!=null)) {
      int ct;
      if((iconvert==prolog.G_true||iconvert==prolog.G_null)
          &&(sbuf.length()>0&&sbuf.charAt(0)!=' ')) {
        try {
          ct=INPUT_INT(Integer.parseInt(sbuf));
        } catch(NumberFormatException e) {
          try {
            ct=termReader.putFloat(sbuf);
          } catch(TypeException ee) {
            if(iconvert==prolog.G_true)
              throw ee;
            else
              ct=prolog.atomTable.newFunctor(sbuf,0);
          }
        }
      } else {
        ct=prolog.atomTable.newFunctor(sbuf,0);
      }
      if(!unify(t,ct))
        return false;
    } else if(isNONVAR(vt)&&(sbuf=name2buf(t,vt,iconvert))!=null) {
      /*
       * int r = getHeapTop() + 1; for (int i=0; i<sbuf.length(); i++) {
       * pushList(INPUT_INT(sbuf.charAt(i)) ); } pushNil();
       */

      int r=string2list(sbuf);

      // prolog.dump(termToString(r));//$$
      if(!unify(r,l))
        return false;
    } else {
      throw new TypeException("Error in name2list, bad arguments: "
          +termToString(t)+","+termToString(l));
    }
    return true;
  }

  /**
   * does not work on $FLOAT
   */
  private String name2buf(int t,int vt,int iconvert) throws TypeException {
    // if (isINTEGER(vt))
    if(prolog.G_true==iconvert||prolog.G_null==iconvert) {
      if(vt==prolog.G_FLOAT) {
        return ""+refToDouble(t);
      } else if(isINTEGER(vt)) {
        return Integer.toString(OUTPUT_INT(vt));
      } else if(prolog.G_true==iconvert) {
        throw new TypeException("number expected in name/2, found:"+termToString(vt));
      }
    }
    if(GETARITY(vt)==0)
      return getAtomName(vt);
    else
      return null;
  }

  private void validKeys2(String f,boolean x3) throws TypeException {
    boolean ok=true;
    if(x3)
      xref=X(3);
    fullDeref();
    if(isVAR(xval)) {
      ok=false;
      f=f+"=>"+dumpCell(xval);
    }
    xref=X(1);
    fullDeref();
    if(isVAR(xval)||xval==prolog.G_predmark)
      ok=false;
    int x1=xval;
    xref=X(2);
    fullDeref();
    if(isVAR(xval))
      ok=false;
    xref=x1;
    // xref and xval contain the 2 keys !!!
    if(!ok)
      throw new TypeException("bad data in "+f+": <"+dumpCell(xref)+","
          +dumpCell(xval)+">");
  }

  // ----------- classes and instances -----------------

  private int instance_id;

  public final int new_instance_id() {
    set_instance_id(prolog.new_instance_id());
    return this.instance_id;
  }

  public final void set_instance_id(int instance_id) {
    this.instance_id=instance_id;
  }

  public final int get_instance_id() {
    return this.instance_id;
  }

  public final int get_class_id() {
    return this.prolog.atomTable.o2i(this.prolog);
  }

  public final String get_class_name() {
    return this.prolog.get_class_name();
  }

  /**
   * Stops this machine
   */

  public final void stop() {
    destroy();
  }
  
  public void destroy() {
    if(!isStopped()) {// otherwise, it is already done!!!
      try {
        destroy0();
      } catch(Throwable e) {
      }
    }
  }

  private void destroy0() {
    regs=null;
    op=END;
    instrPtr=0;
    super.destroy();
    removeObject(this);
    dict=null;
    codeStore=null;
    dict=null;
    unifyStack.destroy();
    choice.destroy();
    trail.destroy();
    undoTable=null;
  }
  
  /**
   * Expects (X:-G), returns the(Answer) or no
   */
  final public Object query_engine(Object X,Object G) {
    Fun query=new Fun(":-",X,G);
    return query_engine(query);
  }

  final public Object query_engine(Object query) {
    Object answer=null;
    try {
      if(load_engine(query)) {
        answer=ask_term();
        if(query instanceof String&&"[]".equals(answer))
          answer=query;
        if(null!=answer) {
          answer=new Fun("the",answer);
        }
      } else {
        stop();
      }
    } catch(Exception e) {
      if(!isStopped())
        Interact.errmes("error in machine.query_engine() on query="+query,e);
    }
    if(null==answer)
      answer="no";
    return answer;
  }

  final public boolean load_engine(Object query) {
    //Prolog.dump(this+"=>LOAD_ENGINE: "+query);
    try {
      INIT_INTERP(query);
      return true;
    } catch(PrologException e) {
      Interact.errmes("error in machine.load_engine():"+this,e);
      stop();
      return false;
    }
  }

  final public Object ask_term() {
    int t=0;
    try {
      t=ask();
    } catch(PrologException e) {
      // ok: t==null
    }
    if(0==t)
      return null;
    return toExternal(t);
  }

  public final void advance_code_top() {
    prolog.advance_code_top();
  }

  /**
   * Queries an interpreter, returning a term, one answer at a time.
   */

  final public boolean isStopped() {
    return null==regs;
  }

  final public int ask() throws PrologException {
    int answer=0;
    if(isStopped())
      return answer; // machine already stopped
    try {
      answer=runInternal();
      //Prolog.dump("HERE isStopped"+isStopped()+" answer"+answer);
    } catch(Exception e) {
      if(!isStopped()) {
        String s="ip="+instrPtr+",cell="+dumpCell(xval);
        Interact.errmes("error in engine "+this+":ask()=>"+s+": ",e);
        stop(); // $$ stop engine on failure happens here
      }
      if(!isStopped()) {
        if(e instanceof PrologException)
          throw ((PrologException)e);
        else {
          throw new PrologException("unexpected exception:"+e);
        }
      }
    }
   
    return answer; // machine could be reused even if answer==0
  }

  final private void arg() {
    int argNo=X(1);
    if(!isINTEGER(argNo)) {
      warn_mes(xval+"arg/3's 1st arg must be integer");
      FAILURE();
      return;
    }
    argNo=OUTPUT_INT(argNo);

    xref=X(2);
    FDEREF();

    if(isATOMIC(xval)) {
      FAILURE();
      return;
    }
    if(isVAR(xval)) {
      warn_mes("arg/3's 2nd arg must be nonvar.");
      FAILURE();
      return;
    }
    if(argNo<=0||argNo>GETARITY(xval)) {
      warn_mes("bad index: "+argNo
          +", arg/3's 1st argument must be in range 1.."+GETARITY(xval)
          +" for predicate "+dumpCell(xval));
      FAILURE();
      return;
    }
    xref+=argNo;
    OUT(xref);
  }

  final void change_arg() {
    int argNo=X(1);
    if(!isINTEGER(argNo)) {
      warn_mes(xval+"change_arg/3's 1st arg must be integer");
      FAILURE();
      return;
    }
    argNo=OUTPUT_INT(argNo);

    xref=X(2);
    FDEREF();

    if(isATOMIC(xval)) {
      FAILURE();
      return;
    }
    if(isVAR(xval)) {
      warn_mes("change_arg/3's 2nd arg must be nonvar.");
      FAILURE();
      return;
    }
    if(argNo<=0||argNo>GETARITY(xval)) {
      warn_mes("bad index: "+argNo
          +", change_arg/3's 1st argument must be in range 1.."+GETARITY(xval)
          +" for predicate "+dumpCell(xval));
      FAILURE();
      return;
    }
    xref+=argNo;
    xval=X(3);
    setRef(xref,xval);
  }

  private final static int JAVA=-4;

  public final static int REAL=-3;

  public final static int INT=-2;

  public final static int VAR=-1;

  public final static int CONST=0;

  private final int compVal(int xval) throws TypeException {
    if(prolog.G_OBJECT==xval)
      ires=JAVA;
    else if(prolog.G_FLOAT==xval)
      ires=REAL;
    else if(isINTEGER(xval))
      ires=INT;
    else if(isVAR(xval))
      ires=VAR;
    else if(isIDENTIFIER(xval))
      ires=GETARITY(xval);
    else
      throw new TypeException("unexpected type: "+termToString(xval));
    return ires;
  }

  private final int ocompare(int l,int vl,int r,int vr) throws TypeException {
    int xl=compVal(vl); // gets "arities"
    int xr=compVal(vr);
    int ires;

    // Prolog.dump("COMPARE TYPES:
    // "+termToString(l)+"/:"+xl+"="+termToString(r)+"/:"+xr);

    if(xl==REAL&&xr==INT) {
      r=toReal(vr);
      xr=REAL;
    } else if(xl==INT&&xr==REAL) {
      l=toReal(vl);
      xl=REAL;
    }

    if(xl==xr) { // same type/arity !!!
      if(xl==REAL)
        ires=compareReals(l,r); // should be FIRST - deep comparison is needed
                                // !!!
      else if(vl==vr)
        ires=0; // ok here ...
      else if(xl==INT)
        ires=OUTPUT_INT(vl)-OUTPUT_INT(vr);
      else if(xl==VAR)
        ires=vl-vr;
      else if(xl==JAVA)
        ires=-1; // first is smaller - avoids removal by sort/2
      else if(xl>=CONST) {
        String sl=getAtomName(vl);
        String sr=getAtomName(vr);
        // if(null==sl) return -1;//throw new TypeException("null object in
        // comparison of <"+sl+"> and <"+sr+">");
        // if(null==sr) return 1;//throw new TypeException("null object in
        // comparison of <"+sl+"> and <"+sr+">");
        // ires==sl.compareTo(sr); //jdk 1.2.x
        int len=Math.min(sl.length(),sr.length());
        if(0==len)
          ires=sl.length()-sr.length();
        else {
          ires=0;
          for(int i=0;i<len;i++) {
            ires=sl.charAt(i)-sr.charAt(i);
            if(ires!=0)
              break;
            if(i==len-1)
              ires=sl.length()-sr.length();
          }
        }
        if(0==ires)
          ires=xl-xr;
      } else
        throw new TypeException("unsolved comparaison: "+termToString(vl)+"=?="
            +termToString(vr));
    } else { // xl!=x
      ires=xl-xr;
    }

    if(ires>0)
      ires=1;
    else if(ires<0)
      ires=-1;
    // Prolog.dump("ires="+ires);
    return ires;
  }

  private final int toReal(int i) {
    double d=(double)OUTPUT_INT(i);
    int r=termReader.putFloat(d);
    // Prolog.dump("toReal:"+termToString(i)+"==>"+s+"==>"+refToDouble(r));
    return r;
  }

  private final double refToDouble(int xref) throws TypeException {
    // if(true) return TermConverter.parseDouble(termToString(fref)); // $$
    // costly !!!
    // return 1.0d;
    if(isVAR(xref)) {
      deref(xref);
      xref=this.xref;
      //xval=this.xval;
    } else
      this.xval=xref;
    if(xval==prolog.G_FLOAT) { // $float
      deref(xref+1);
      int i1=this.xval;
      deref(xref+2);
      int i2=this.xval;
      deref(xref+3);
      int res=this.xval;
      return TermConverter.toDouble(i1,i2,res);
    } else {
      throw new TypeException("number expected in refToDouble, got: "+termToString(xref));
    }
  }

  private final int compareReals(int l,int r) throws TypeException {

    double dl=refToDouble(l);
    double dr=refToDouble(r);
    int ires;
    if(dl<dr)
      ires=-1;
    else if(dl>dr)
      ires=1;
    else
      ires=0;
    // Prolog.dump("COMPARE FLOATS:"+dl+"-"+dr+"=>"+ires);
    return ires;
  }

  private final int compare(int l,int r) throws TypeException {
    int ires;
    int vl,vr;
    if(isVAR(l)) {
      deref(l);
      l=xref;
      vl=xval;
    } else
      vl=l;
    if(isVAR(r)) {
      deref(r);
      r=xref;
      vr=xval;
    } else
      vr=r;
    ires=ocompare(l,vl,r,vr);
    if(0==ires&&isCOMPOUND(vl)&&REAL!=compVal(vl)&&REAL!=compVal(vr))
      for(vr=GETARITY(vl),vl=1;0==ires&&vl<=vr;vl++)
        ires=compare(l+vl,r+vl);
    return ires;
  }

  private final boolean float_rel(int relop,int x,int y) throws TypeException {
    // TODO: check x,y are numerical, floats or ints
    int r=compare(x,y);
    // Prolog.dump("rel:"+relop+" =>"+r);
    boolean succeeds;
    switch(relop) {
    case LESS_2:
      succeeds=(r<0);
      break;
    case GREATER_2:
      succeeds=(r>0);
      break;
    case LESS_EQ_2:
      succeeds=(r<=0);
      break;
    case GREATER_EQ_2:
      succeeds=(r>=0);
      break;
    case ARITH_EQ_2:
      succeeds=(r==0);
      break;
    case ARITH_DIF_2:
      succeeds=(r!=0);
      break;
    default:
      throw new TypeException("Error: Undefined relational operator:"+relop);
    }
    return succeeds;
  }

  public int addObject(Object O) {
    return prolog.atomTable.addObject(O);
  }

  public boolean removeObject(Object O) {
    return prolog.atomTable.removeObject(O);
  }

  /*
   * public HeapRef hash2alist(ObjectDict htable, String cons) throws
   * PrologException { return new HeapRef(termReader.hash2alist(htable, cons)); }
   */

  public int jcall(int X1,int X2) throws PrologException {
    if(!isSYMCONST(X1)) {
      throw new TypeException("Function name expected as arg(1) of jcall to: "
          +dumpCell(X1));
    }
    String f=getAtomName(X1);
    // Prolog.dump("here:" +f);
    if("hkey".equals(f)) {
      if(isVAR(X2))
        X2=getRef(X2);
      return prolog.atomTable.hkey(X2);
    }

    if("term_hash".equals(f)) {
      // if(isVAR(X2)) X2=getRef(X2);
      return termHash(X2);
      // return prolog.atomTable.hkey(X2);
    }

    if(isINTEGER(X2)) {
      int ires=jcall_int(f,OUTPUT_INT(X2));
      return INPUT_INT(ires);
    }

    if(!isSYMCONST(X2)) {
      throw new TypeException("String expected as arg(2) of jcall to:"+f
          +" ,found:"+dumpCell(X2));
    }
    String x=getAtomName(X2);

    String r="yes";

    try {
      if("get_prolog_class".equals(f)) {
        Prolog P=prolog.getPrologClass(x);
        if(null==P)
          r=prolog.S_null;
        else
          r=""+addObject(P);
      } else if("get_class_name".equals(f))
        r=get_class_name();

      else if("user_path".equals(f))
        r=""+addObject(Interact.USER_PATH);

      else if("add_to_path".equals(f))
        Interact.add_to_path(x);
      else if("push_to_path".equals(f))
        Interact.push_to_path(x);
      else if("del_from_path".equals(f))
        Interact.del_from_path(x);
      else if("clear_path".equals(f))
        Interact.clear_path();
      else if("path_element".equals(f)) {
        r=Interact.path_element(Integer.parseInt(x));
      } else if("advance_code_top".equals(f))
        prolog.advance_code_top();
      else if("rollback".equals(f))
        prolog.rollback();
      else
        r=f+"("+x+")=?";
    } catch(Exception e) {
      Interact.errmes("error in jcall: "+f,e);
      r=prolog.S_null;
    }
    return prolog.atomTable.newFunctor(r,0); // string return
  }

  boolean isUNBOUND(int ref) {
    this.deref(ref);
    return isVAR(this.xval)&&this.xval==this.xref;
  }

  int jcall_int(String f,int x) throws SystemException {
    if("get_instance_id".equals(f)) {
      Object O=prolog.atomTable.i2o(x);
      return ((LogicEngine)O).get_instance_id();
    } else if("new_instance_id".equals(f)) {
      Object O=prolog.atomTable.i2o(x);
      if(O instanceof LogicEngine) {
        return ((LogicEngine)O).new_instance_id();
      } else {
        return ((Prolog)O).new_instance_id();
      }
    } else if("set_instance_id".equals(f)) {
      set_instance_id(x);
      return x;
    } else if("random_seed".equals(f)) {
      prolog.setRandom(x);
      return x;
    } else if("halt".equals(f)) {
      Interact.halt(x);
      return x;
    } else if("shutdown".equals(f)) {
      Interact.halt(x);
      return x;
    } else
      throw new SystemException("error in jcall_int");
  }

  synchronized final public void run() {
    int result=0;
    // System.err.println("HERER$$$");
    try {
      result=ask();
    } catch(PrologException e) {
      // nothing to do ask() has stop()-ed this machine already
      Interact.warnmes(e.toString());
    } catch(Throwable e) {
      if(!isStopped()) {
        Interact.errmes("fatal error in Prolog engine: "+this,e);
        if(Interact.quickfail>=1) 
          Interact.halt(1);
        else 
          stop();
      }
    }
    if(0!=result)
      Interact.warnmes("error in bg: "+termToString(result));
    stop();
  }

  ////
  synchronized private void bg() throws PrologException {
    if(!isINTEGER(X(1)))
      throw new TypeException("bad 1-st arg in run_bg/2: "+termToString(X(1)));

    ires=OUTPUT_INT(X(1));
    Object O=prolog.atomTable.i2o(ires);

    if(null==O) {
      throw new TypeException("1-st arg in run_bg/2 should be an engine: "
          +termToString(X(1)));
    } else {
      LogicEngine M=(LogicEngine)O;
      Thread T=new Thread(M,"PrologThread");
      T.setDaemon(true);
      xval=termReader.putObject(T);
      // xval=INPUT_INT(ires);
      T.start();
    }
    OUT(xval);
  }
  
  private synchronized void engine_op(int op) throws PrologException {
    switch(op) {
    case CREATE_ENGINE_3: {
      // Prolog.dump("suspect cell in new_engine:"+termToString(X(1)));
      // int t=termReader.make_fun2(":-",X(1),X(2));
      xval=X(1);

      boolean LOAD_ENGINE=false;
      Object Handle=null;
      if(isINTEGER(xval)) {
        ires=OUTPUT_INT(xval);
        if(ires<0) {
          ires=-ires;
          LOAD_ENGINE=true;
        }
        Handle=prolog.atomTable.i2o(ires);
      } else
        throw new TypeException("bad 1-st arg in create_engine/3: "
            +termToString(X(1)));
      int t=X(2);
      EncodedTerm T=encodedCopy(t);

      LogicEngine M;
      if(LOAD_ENGINE) {
        M=(LogicEngine)Handle;
      } else {
        Prolog P=(Prolog)Handle;
        M=newLogicEngine(P); // sometime a Machine
        M.set_instance_id(instance_id);
        ires=P.atomTable.addObject(M);
      }
      M.INIT_INTERP(T);

      xval=INPUT_INT(ires);
      OUT(xval);

      return;
    }

    case ENGINE_GET_2: {
      // Prolog.dump("!!!engine_get:"+termToString(X(1)));

      if(!isINTEGER(X(1)))
        throw new TypeException("bad 1-st arg in get/2: "+termToString(X(1)));

      ires=OUTPUT_INT(X(1));
      Object O=prolog.atomTable.i2o(ires);

      if(null==O) {
        xval=prolog.atomTable.newFunctor("no",0);
      } else if(impure_get(O)) {
      } // nothing
      else {
        // Prolog.dump("engine_get: machine:"+O+":"+O.getClass());
        try {
          LogicEngine M=(LogicEngine)O;
          int result=M.ask();
          if(0==result) {
            removeObject(M);
            M.stop(); // $$ stop on failure happens here
            xval=prolog.atomTable.newFunctor("no",0);
          } else {
            xref=copyTermFrom(M,result);
            xval=prolog.atomTable.newFunctor("the",1);
            xval=pushTerm(xval); // builds the(X)
            pushTerm(xref);
          }
        } catch(PrologException e) {
          xval=prolog.atomTable.newFunctor("no",0);
        } catch(ClassCastException e) {
          // Interact.warnmes("get/2 attempted on unknown object: "+O);
          xval=prolog.atomTable.newFunctor("no",0);
          throw new TypeException("get/2 atempted on unknown object: "+O);
        }
      }

      OUT(xval);
      return;
    }

    case ENGINE_STOP_1: {
      // Prolog.dump("!!!engine_stop:"+termToString(X(1)));
      ires=OUTPUT_INT(X(1));
      Object O=prolog.atomTable.i2o(ires);
      if(null!=O) {
        if(O instanceof LogicEngine) {
          ((LogicEngine)O).stop(); // also removes from its Prolog's internal
                                    // table
        } else if(O instanceof Prolog)
          ((Prolog)O).stop(); // also removes from its Prolog's internal table
        else if(stop_impure(O)) { // O is something like PrologReader etc.
        } // nothing
        else {
          throw new TypeException(
              "bad 1-st arg in stop/1: no stop method for: "+termToString(X(1))
                  +"==>"+O+"??");
        }
      } else { // O == null
        throw new TypeException(
            "bad 1-st arg in stop/1: not a known object reference: "+ires);
      }

      instrPtr++;
      return;
    }
    default: {
    }
    }
  }

  protected boolean stop_impure(Object O) {
    stub("stop_impure");
    return false;
  }

  protected boolean impure_get(Object O) throws PrologException {
    stub("impure_get");
    return false;
  }

  final void to_engine(int m,int t) throws ExistenceException,TypeException {
    if(!isINTEGER(m))
      throw new TypeException("integer engine handle expected, instead of: "
          +dumpCell(m));
    ires=OUTPUT_INT(m);
    LogicEngine M=(LogicEngine)prolog.atomTable.i2o(ires);

    if(!M.putMessage(encodedCopy(t)))
      throw new ExistenceException("to_engine("+dumpCell(m)
          +"): full messageBox");
  }

  final int from_engine() throws ExistenceException {
    if(null==messageBox)
      throw new ExistenceException("from_engine: empty messageBox found");
    int res=decodedCopy(messageBox);
    messageBox=null;
    return res;
  }

  boolean putMessage(EncodedTerm T) {
    if(null!=messageBox)
      return false;
    messageBox=T;
    return true;
  }

  final Object int2ob(int i) {
    int ires=OUTPUT_INT(i);
    return prolog.atomTable.i2o(ires);
  }

  
  
   public final boolean exportBundle(int cloned) {
    if(null!=this.ioBundle || null==this.messageBox) return false;
    EncodedTerm T=this.messageBox;
    this.messageBox=null; // taken!
    this.ioBundle=(new PortableTerm(T,prolog)).export(cloned);
    return true;
  }

  public final boolean importBundle(int cloned) {
    if(null==this.ioBundle || null!=this.messageBox) return false;
    PortableTerm P=new PortableTerm(this.ioBundle,cloned);
    this.ioBundle=null; // taken!
    messageBox=P.outputTo(prolog);
    return true;
  }
  
  public final void setBundle(Object[] bundle) {
    this.ioBundle=bundle;
  }
  
  public final Object[] getBundle() {
    return this.ioBundle;
  }
  
  final Object ref2term(int x) {
    EncodedTerm T=encodedCopy(x);
    return new PortableTerm(T,prolog);
  }

  final int term2ref(Object O) {
    PortableTerm P=(PortableTerm)O;
    EncodedTerm T=P.outputTo(prolog);
    return decodedCopy(T);
  }

  public final int pushTermList(Object[] os) {
    int l=os.length;
    int[] is=new int[l];

    for(int i=0;i<l;i++) {
      is[i]=term2ref(os[i]);
    }

    int h=getHeapTop()+1;
    for(int i=0;i<l;i++) {
      pushList(is[i]);
    }
    pushTerm(prolog.G_NIL);

    return h;
  }

  private final void makeUndoTable() {
    if(null==undoTable)
      undoTable=new ObjectDict();
  }

  public final void add_undoable(Object O) {
    ((ChoicePointStackEntry)choice.peek()).add((Undoable)O);
  }

  public final void uput(String key,Object term) {
    makeUndoTable();
    if(null==undoTable.get(key)) {
      UndoKey ukey=new UndoKey(key,this);
      ((ChoicePointStackEntry)choice.peek()).add(ukey);
    }
    undoTable.put(key,term);
  }

  public final Object uget(Object key) {
    if(null==undoTable)
      return null;
    return undoTable.get(key);
  }

  public final void uremove(Object key) {
    if(null==undoTable)
      return;
    undoTable.remove(key);
  }

  void bb_op(int op) throws PrologException {
    switch(op) {
    case DEF_3:
      validKeys2("def/3",true);
      if(!dict.do_hdef(xref,xval,X(3),this)) {
        FAILURE();
        break;
      }
      instrPtr++;
      break;

    case SET_3:
      validKeys2("set/3",true);
      dict.do_hset(xref,xval,X(3),this);
      instrPtr++;
      break;

    case VAL_3:
      validKeys2("val/3",false);
      xval=dict.do_hget(xref,xval,this);
      if(dict.do_isEmpty(xval)) {
        FAILURE();
        break;
      }
      OUT(xval);
      break;

    case RM_2:
      validKeys2("rm/2",false);
      dict.do_hremove(xref,xval);
      instrPtr++;
      break;

    default: {
    }
    }
  }

  void queue_op(int op) throws ResourceException {
    switch(op) {
    case QUEUE_CREATE_1: {
      ires=addObject(new ObjectQueue());
      xval=INPUT_INT(ires);
      if(xval==0) {
        throw new ResourceException("failing to create Queue");
        // FAILURE();
      } else
        OUT(xval);
    }
      break;

    case QUEUE_SIZE_2: {
      ObjectQueue Q=(ObjectQueue)int2ob(X(1));
      ires=Q.size();
      xval=INPUT_INT(ires);
      OUT(xval);
    }
      break;

    case QUEUE_OP_4: {
      ObjectQueue Q=(ObjectQueue)int2ob(X(1));
      xval=OUTPUT_INT(X(2));
      ires=OUTPUT_INT(X(3));
      Object E=Q.queue_op4(xval,ires);
      xval=(null==E)?prolog.G_null:term2ref(E);
      OUT(xval);
    }
      break;

    case QUEUE_DESTROY_1: {
      ObjectQueue Q=(ObjectQueue)int2ob(X(1));
      Q.clear();
      this.removeObject(Q);
      instrPtr++;
    }
      break;

    case QUEUE_ADD_2: {
      ObjectQueue Q=(ObjectQueue)int2ob(X(1));
      Object E=ref2term(X(2));
      Q.enq(E);
      instrPtr++;
    }
      break;

    case QUEUE_POP_2: {
      ObjectQueue Q=(ObjectQueue)int2ob(X(1));
      Object E=Q.deq();
      xval=term2ref(E);
      OUT(xval);
    }
      break;

    case QUEUE_PUSH_2: {
      ObjectQueue Q=(ObjectQueue)int2ob(X(1));
      Object E=ref2term(X(2));
      Q.pushq(E);
      instrPtr++;
    }
      break;

    case QUEUE_UPDATE_AT_3: {
      ObjectQueue Q=(ObjectQueue)int2ob(X(1));
      ires=OUTPUT_INT(X(2));
      Object E=ref2term(X(3));
      Q.updateAt(ires,E);
      instrPtr++;
    }
      break;

    case QUEUE_LIST_2: {
      ObjectQueue Q=(ObjectQueue)int2ob(X(1));
      xval=pushTermList(Q.toArray());
      OUT(xval);
    }
      break;

    default: {
    }
    }
  }

  synchronized boolean stats(int op) {
    switch(op) {
    case RUNTIME_2: {
      int time=(int)(System.currentTimeMillis()-prolog.startTime);
      if(putArg(1,INPUT_INT(time))) {
        putLastArg(2,INPUT_INT(time-prolog.rtime));
      }
      prolog.rtime=time;
    }
      break;

    case GLOBAL_STACK_2:
      if(putArg(1,INPUT_INT(getHeapTop())))
        putLastArg(2,INPUT_INT(getFree()));
      break;

    case LOCAL_STACK_2:
      xref=INPUT_INT(choice.size());
      if(!unify(xref,regs[1])) {
        return false;
      }
      xref=INPUT_INT(choice.getFree());
      if(!unify(xref,regs[2])) {
        return false;
      }

      regs[1]=regs[3];
      instrPtr++;
      break;

    case TRAIL_2:
      if(putArg(1,INPUT_INT(trail.getTop()+1)))
        putLastArg(2,INPUT_INT(trail.getFree()));
      break;

    case CODE_2:
      if(putArg(1,INPUT_INT(codeStore.getUsed())))
        putLastArg(2,INPUT_INT(codeStore.getFree()));
      break;

    case STRINGS_2: // in java their is no string storage area - so report dict
                    // values instead.
    case SYMBOLS_2:
      // prolog.atomTable.dump_last();
      if(putArg(1,INPUT_INT(prolog.atomTable.size())))
        putLastArg(2,INPUT_INT(prolog.atomTable.capacity()));
      break;

    // statiscs on usage
    case HTABLE_2:
      if(putArg(1,INPUT_INT(dict.getUsed())))
        putLastArg(2,INPUT_INT(dict.getFree()));
      break;

    default: {
    }
    }
    return true;
  }

  private void show_trace() {
    prolog.instrCount++; // TRACE
    codeStore.dumpInstruction(prolog.instrCount,instrPtr);
    Interact.warnmes("\tAn: R"+LCGET(curInstr)+"="+An()+", Ai: R"
        +MCGET(curInstr)+"="+Ai());
  }

  private void trace_on() {
    prolog.instrCount=0;
    prolog.TRACE_START=1;
    prolog.TRACE_STOP=12;
    prolog.TRACE_ON=!prolog.TRACE_ON;
  }

  protected void write_nl_io(int op) {
    stub("write_nl_io:"+op);
  }

  protected void byte_io(int op) throws TypeException {
    stub("byte_io:"+op);
  }

  protected void string_io(int op) throws PrologException {
    stub("string_io:"+op);
  }

  protected void file_io(int op) throws PrologException {
    stub("file_io:"+op);
  }

  protected void seen_told_io(int op) {
    stub("seen_told_io:"+op);
  }

  protected void destroy_io() {
    stub("destroy_io");
  }

  protected void new_fluent() throws PrologException {
    stub("new_fluent");
  }

  protected void refl_op(int op) {
    stub("refl_op:"+op);
  }

  protected void shell() throws PrologException {
    stub("shell");
  }

  public void serialize(int hRef) throws PrologException {
    stub("serialize");
  }
}
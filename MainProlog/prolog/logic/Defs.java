package prolog.logic;

/** Definition of Prolog instruction codes and static data transformer methods.
* This interface describes only WAM instructions.
* The definitions for builtin predicates can be found in {@link BuiltinIDs BuiltinIDs}.
*<p>The Prolog engine defines a set of registers which the following instructions manipulate.
* A small set of temporary registers is also defined. These are indicated by X(i) below.
*<p>Instructions can be divided into one word, two or three word formats.
* The first word (WORD1) has the following layout:
*<p>
* <table border=1>
* <tr><td>REG id<td>ARG id<td>OP id
* <tr><td>11 bits<td>11 bits<td>9 bits
* </table>
*<p>
* Where
*<ul>
*<li>REG is either the index of a WAM register, or an immediate operand(depending on instruction).
*<li>ARG is the index of a WAM register.
*<li>OP is the instruction id, from the list below augmented by the list of builtin operations.
*</ul>
*<p>
* The format of the second, or third instruction words (WORD2, WORD3 respectively) depends on the instruction.
*
*<p>The following notations are used below:
*<table>
*<tr><td>&middot;<td>An()<td>The contents of the register indicated by the REG instruction field.
*<tr><td>&middot;<td>Ai()<td>The contents of the register indicated by the ARG instruction field.
*<tr><td>&middot;<td>newvar()<td>Push a new unbound variable onto the heap.
*<tr><td>&middot;<td>heap()<-expr<td>Push expression onto the heap.
*<tr><td>&middot;<td>currentArg()<td>Heap index for current argument of the current predicate/functor.
*<tr><td>&middot;<td>nextArg()<td>Heap index for argument following currentArg for the current predicate/functor.
*<tr><td>&middot;<td>noCurrentArg()<td>No currentArg is defined at this point.
*<tr><td>&middot;<td>cutB<td>The index of the choice point which should be restored on failure.
*</table>
*
*<p>
* Note:
<br>Some instructions only appear in the compiled format. They are resolved by the
* loader into other runtime instructions (eg CLAUSE).
*<br> Compound instructions do not exist in the compiled format. For efficiency they
* are produced by the loader to represent in compressed form a given sequence of 
* basic instructions.
*
*<p>Unless otherwise noted instruction execution continues with the next compiled instruction.
*/

public class Defs implements Stateful { // abstract

  public static final int END = 0;

  /** Unify unbound An() with currentArg.
  *<p>This is equivalent to An()<-currentArg.
  *<p>If noCurrentArg perform {@link InstructionIDs#WRITE_VARIABLE WRITE_VARIABLE} instead.
  */
  static final int UNIFY_VARIABLE = 1;

  /** An()<-newvar().
  *<p> Set register An() to point to a new unbound variable allocated from the heap.
  */
  static final int WRITE_VARIABLE = 2;

  /** Unify An() with currentArg.
  *<p>Fail if An() and currentArg don't unify.
  *<p>If noCurrentArg perform {@link InstructionIDs#WRITE_VALUE WRITE_VARLUE} instead.
  */
  static final int UNIFY_VALUE = 3;

  /** heap()<-An(). */
  static final int WRITE_VALUE = 4;

  /** Unify currentArg with constant term.
  *<p>The constant term is retrived from WORD2.
  *<p>If noCurrentArg perform {@link InstructionIDs#WRITE_CONSTANT WRITE_CONSTANT} instead.
  */
  static final int UNIFY_CONSTANT = 5;

  /** heap()<-constant term.
  * The constant term is stored in WORD2.
  */
  static final int WRITE_CONSTANT = 6;

  /** Bind An() to a constant.
  *<p>If An() dereferences to an unbound variable then bind it to the constant term in WORD2.
  * The variable is optionally trailed.
  *<p> Otherwise fail if the values differ.
  */
  static final int GET_CONSTANT = 7;
  static final int GET_STRUCTURE = 8;

  /** An()<-constant term.
  *<p> The constents of the An() register are set to the value of WORD2 which contains
  * a constant term.
  */
  static final int PUT_CONSTANT = 9;

  /** An()<-heap()<-functor term.
  *<p> A functor term stored in WORD2 is pushed onto the heap. The An() register is set to the new heap index of this functor term.
  */
  static final int PUT_STRUCTURE = 10;

  /** An()<-Ai().
  * Move contents of Ai() register to An() register.
  */
  static final int MOVE_REG = 11;

  /** An()<-newVar(), Ai()<-An(). */
  static final int PUT_VARIABLE = 12;

  /** Bind An() and Ai() registers.
  *<p>Fail if An() and Ai() don't unify. 
  */
  static final int GET_VALUE = 13;

  /** heap()<-cutB.
  * Push the cutB choice point index onto the heap.
  */
  static final int PUSH_CUT = 14;

  /** Set the current choice point back to that saved in cutB. */
  static final int PUT_CUT = 15;

  /** Set the current choice point back to that saved in the dereferenced contents of regs[1]. */
  static final int GET_CUT = 16;

  /** Transfer control to the instructions for a particular predicate.
  *<br>cutB is set to the current choice point.
  *<br>WORD2 contains the instruction index for the required predicate.
  *<br>At load time the instruction index is set by resolving the predicate name/arity.
  *<br>Execution contines at the specified instruction.
  */
  static final int EXECUTE = 17;
  static final int PROCEED = 18;
  /**
  * This instruction indicates the start of a new clause in the compiled instructions.
  * During loading this instruction is replaced by {@link InstructionIDs#TRY_ME_ELSE TRY_ME_ELSE},
  * {@link InstructionIDs#RETRY_ME_ELSE RETRY_ME_ELSE} or {@link InstructionIDs#TRUST_ME_ELSE TRUST_ME_ELSE} instructions.
  *<p>This instruction is not present at run time.
  */
  static final int CLAUSE = 254;
  static final int FIRSTARG = 255;

  /** First clause of a non-detrministic predicate.
  *<p>Create a new choice point.
  *<br>The REG field indicates the arity of the predicate.
  *<br>WORD2 contains the instruction index for the first clause of this predicate.
  *<br>Jump to the new instruction.
  */
  static final int TRY_ME_ELSE = 19;

  /** Middle clauses of a non-detrministic predicate.
  * Backtrack to try another clause. 
  *<br>The REG field indicates the arity of the predicate - but is not used by this implementation
  * as the choice point retains this value.
  *<br>WORD2 contains the instruction index for the next clause of this predicate.
  *<br>Jump to the new instruction.
  */
  static final int RETRY_ME_ELSE = 20;

  /** Last clause of a non-detrministic predicate.
  * Backtrack to the choice point prior to the one set by TRY_ME_ELSE.
  * Hence get Tail Recursion Optimization.
  *<br>The REG field indicates the arity of the predicate - but is not used by this implementation
  * as the choice point retains this value.
  *<br>WORD2 contains the instruction index for the last clause of this predicate.
  *<br>Jump to the new instruction.
  */
  static final int TRUST_ME_ELSE = 21;

  static final int TRY_ME_ONLY = 22;
  static final int NONDET = 23;

  /** This instruction combines {@link InstructionIDs#EXECUTE EXECUTE}
  * and {@link InstructionIDs#SWITCH SWITCH} instructions.
  *<p>This instruction is produced by the loader.
  */
  static final int EXEC_SWITCH = 24;

  /** Transfer control to the clause determined by the value of reg[1].
  * The SWITCH instruction is stored in the code store in the following format:
  *<p>
  *<table border=1>
  *<tr><td>REG unused<td>ARG unused<td>SWITCH
  *<tr><td>WORD1<td colspan=2>predicate id.
  *<tr><td>WORD2<td colspan=2>destination instruction if reg[1]<sup>*</sup> is an unbound variable.
  *</table>
  * * refers to the dereferenced value.
  *<p> If reg[1] is an unbound variable continue execution at the instruction index in WORD2.
  *<p> Otherwise lookup the value stored for the combined predicate id. in WORD1
  * and the dereferenced value of reg[1]. This yields the instruction index of the target clause.
  * Execution continues at this instruction.
  * The operation fails if an unknown value is encountered.
  */
  static final int SWITCH = 25;

  /** This instruction combines {@link InstructionIDs#EXECUTE EXECUTE} and {@link InstructionIDs#JUMP_IF JUMP_IF} instructions.
  *<p>This instruction is produced by the loader.
  */
  static final int EXEC_JUMP_IF = 26;

  /** Transfer control depending on the contents of reg[1].
  * The JUMP_IF instruction is stored in the code store in the following format:
  *<p>
  *<table border=1>
  *<tr><td>REG<td>ARG<td>JUMP_IF
  *<tr><td>WORD1<td colspan=2>test term value
  *<tr><td>WORD2<td colspan=2>destination instruction if reg[1]<sup>*</sup> is an unbound variable.
  *<tr><td>WORD3<td colspan=2 rowspan=2>destination instruction if reg[1]<sup>*</sup> equals the test term.
  *		<br>An()<-??, Ai()<-??.
  *</table>
  * * refers to the dereferenced value.
  *<p>Otherwise execution continues with the next instruction.

  */
  static final int JUMP_IF = 27;

  /** X(REG)<-constant term.
  *<p> A constant term is placed in temporary register X(REG).
  * Here REG indictes the index of a temporary register rather than the usual An() register.
  */
  static final int LOAD_CONSTANT = 28;

  /** X(REG)<-Ai().
  *<p> The dereferenced value of register Ai() is placed in temporary register X(REG).
  * Here REG indictes the index of a temporary register rather than the usual An() register.
  */
  static final int LOAD_VALUE = 29;

  static final int GET_UNIFY_VAR_VAR = 30;

  /** Unify unbound An() with currentArg and unbound Ai() with nextArg.
  *<p>This is equivalent to An()<-currentArg, Ai()<-nextArg.
  *<p>If noCurrentArg perform {@link InstructionIDs#WRITE_VAR_VAR WRITE_VAR_VAR} instead.
  *<p>This instruction is produced by the loader.
  */
  static final int UNIFY_VAR_VAR = 31;

  /** An()<-heap()<-functor term, An()<-newvar(), Ai()<-newvar().
  *<p>This is a combination of {@link InstructionIDs#PUT_STRUCTURE PUT_STRUCTURE}
  * and {@link InstructionIDs#WRITE_VAR_VAR WRITE_VAR_VAR}.
  *<p>WORD2 contains the functor term.
  *<p>WORD3 is formated like WORD1 to provide the An() and Ai() references.
  */
  static final int PUT_WRITE_VAR_VAR = 32;

  /** An()<-newvar(), Ai()<-newvar(). */
  static final int WRITE_VAR_VAR = 33;

  static final int GET_UNIFY_VAL_VAL = 34;

  /** Unify An() with currentArg and Ai() with nextArg.
  *<p>If noCurrentArg perform {@link InstructionIDs#WRITE_VAR_VAR WRITE_VAL_VAL} instead.
  *<p>This instruction is produced by the loader.
  */
  static final int UNIFY_VAL_VAL = 35;

  /** An()<-heap()<-functor term, heap()<-An(), heap()<-Ai().
  *<p>This is a combination of {@link InstructionIDs#PUT_STRUCTURE PUT_STRUCTURE}
  * and {@link InstructionIDs#WRITE_VAL_VAL WRITE_VAL_VAL}.
  *<p>WORD2 contains the functor term.
  *<p>WORD3 is formated like WORD1 to provide the An() and Ai() references.
  */
  static final int PUT_WRITE_VAL_VAL = 36;

  /** heap()<-An(), heap()<-Ai(). */
  static final int WRITE_VAL_VAL = 37;

  static final int GET_UNIFY_VAR_VAL = 38;

  /** Unify unbound An() with currentArg and Ai() with nextArg.
  *<p>If noCurrentArg perform {@link InstructionIDs#WRITE_VAR_VAL WRITE_VAR_VAL} instead.
  *<p>This instruction is produced by the loader.
  */
  static final int UNIFY_VAR_VAL = 39;

  /** An()<-heap()<-functor term, An()<-newvar(), heap()<-Ai().
  *<p>This is a combination of {@link InstructionIDs#PUT_STRUCTURE PUT_STRUCTURE}
  * and {@link InstructionIDs#WRITE_VAR_VAL WRITE_VAR_VAL}.
  *<p>WORD2 contains the functor term.
  *<p>WORD3 is formated like WORD1 to provide the An() and Ai() references.
  */
  static final int PUT_WRITE_VAR_VAL = 40;

  /** An()<-newvar(), heap()<-Ai(). */
  static final int WRITE_VAR_VAL = 41;


  static final int GET_UNIFY_VAL_VAR = 42;

  /** Unify An() with currentArg and unbound Ai() with nextArg.
  *<p>If noCurrentArg perform {@link InstructionIDs#WRITE_VAL_VAR WRITE_VAL_VAR} instead.
  *<p>This instruction is produced by the loader.
  */
  static final int UNIFY_VAL_VAR = 43;

  /** An()<-heap()<-functor term, heap()<-An(), Ai()<-newvar().
  *<p>This is a combination of {@link InstructionIDs#PUT_STRUCTURE PUT_STRUCTURE}
  * and {@link InstructionIDs#WRITE_VAL_VAR WRITE_VAL_VAR}.
  *<p>WORD2 contains the functor term.
  *<p>WORD3 is formated like WORD1 to provide the An() and Ai() references.
  */
  static final int PUT_WRITE_VAL_VAR = 44;

  /** heap()<-An(), Ai()<-newvar(). */
  static final int WRITE_VAL_VAR = 45;

  /** An()<sub>1</sub><-Ai()<sub>1</sub>, An()<sub>2</sub><-Ai()<sub>2</sub>.
  *<p>A pair of MOVE_REG instructions coded as one instruction.
  *<br>Move contents of Ai() register to An() register.
  *<br>The contents of WORD2 are then interpreted in WORD1 format yielding a new set of register references.
  *<br>The contents of the new Ai() register are moved to the new An() register.
  * @see InstructionIDs#MOVE_REG MOVE_REG
  */
  static final int MOVE_REGx2 = 46;

  /** X(1)<-Ai(), X(2)<-An().
  *<br> The dereferenced value of register Ai() is placed in temporary register X(1).
  *<br> The dereferenced value of register An() is placed in temporary register X(2).
  */
  static final int LOAD_VALUEx2 = 47;

  /** X(1)<-Ai(), X(2)<-a small integer.
  *<br> The dereferenced value of register Ai() is placed in temporary register X(1).
  *<br> A small integer derived from the contents of REG field/An() is placed in temporary register X(2).
  */
  static final int LOAD_VAL_SHORT = 48;

  /** This instruction combines {@link InstructionIDs#EXECUTE EXECUTE}
  * and {@link InstructionIDs#TRY_ME_ELSE TRY_ME_ELSE} instructions.
  *<p>This instruction is produced by the loader.
  */
  static final int EXEC_TRY = 49;

  /******************************
  * Builtins start here 
  *******************************/
  static final int INLINE = 50;

  static final int FAIL_0 = 50;
  protected static final int CWRITE_1 = 51;
  protected static final int NL_0 = 52;
  static final int VAR_1 = 53;
  static final int NONVAR_1 = 54;
  static final int INTEGER_1 = 55;
  static final int ATOMIC_1 = 56;
  static final int IS_COMPILED_1 = 57;
  static final int RETURN_1 = 58;
  protected static final int SEEN_0 = 59;
  protected static final int TOLD_0 = 60;

  static final int ARITH = 61;
  static final int PLUS_3 = 61;
  static final int SUB_3 = 62;
  static final int MUL_3 = 63;
  static final int DIV_3 = 64;
  static final int MOD_3 = 65;
  static final int RANDOM_1 = 66;
  protected static final int GET0_1 = 67;
  protected static final int PUT0_1 = 68;
  static final int LESS_2 = 69;
  static final int GREATER_2 = 70;
  static final int LESS_EQ_2 = 71;
  static final int GREATER_EQ_2 = 72;
  static final int ARITH_EQ_2 = 73;
  static final int ARITH_DIF_2 = 74;
  static final int LSHIFT_3 = 75;
  static final int RSHIFT_3 = 76;
  static final int L_AND_3 = 77;
  static final int L_OR_3 = 78;
  static final int L_XOR_3 = 79;
  static final int L_NEG_3 = 80;
  static final int COMPARE0_3 = 81;
  static final int ARG_3 = 82;
  static final int DEF_3 = 83;
  static final int SET_3 = 84;
  static final int VAL_3 = 85;
  static final int RM_2 = 86;

  static final int NAMECAT_4 = 87;
  static final int COPY_TERM_2 = 88;
  static final int NEW_CODE_2 = 89;

  protected static final int SEEING_1 = 90;
  protected static final int TELLING_1 = 91;
  protected static final int SEE_OR_FAIL_1 = 92;
  protected static final int TELL_OR_FAIL_1 = 93;

  static final int ADD_INSTR_5 = 94;
  static final int DET_APPEND_3 = 95;

  static final int CREATE_ENGINE_3 = 96;
  static final int ENGINE_GET_2 = 97;
  static final int ENGINE_STOP_1 = 98;

  static final int NEW_FLUENT_3 = 99;

  protected static final int SREAD0_4 = 100;
  protected static final int SWRITE_2 = 101;

  protected static final int NEW_JAVA_CLASS_2 = 102;
  protected static final int NEW_JAVA_OBJECT_3 = 103;
  protected static final int INVOKE_JAVA_METHOD_5 = 104;
  protected static final int DELETE_JAVA_CLASS_2 = 105;
  protected static final int DELETE_JAVA_OBJECT_2 = 106;
  protected static final int GET_JAVA_FIELD_HANDLE_3 = 107;

  static final int RUN_BG_2 = 108;
  static final int CURRENT_ENGINE_1 = 109;
  static final int TO_ENGINE_2 = 110;
  static final int FROM_ENGINE_1 = 111;

  static final int JCALL_3 = 112;

  static final int QUEUE_CREATE_1 = 113;
  static final int QUEUE_SIZE_2 = 114;
  static final int QUEUE_OP_4 = 115;
  static final int QUEUE_DESTROY_1 = 116;
  static final int QUEUE_ADD_2 = 117;
  static final int QUEUE_POP_2 = 118;
  static final int QUEUE_PUSH_2 = 119;
  static final int QUEUE_UPDATE_AT_3 = 120;
  static final int QUEUE_LIST_2 = 121;
  static final int CHANGE_ARG_3 = 122;

  static final int BUILTIN = 123;
  static final int DEMO_0 = BUILTIN + 0;
  static final int DEMO_1 = BUILTIN + 1;
  static final int CLASS_NAME_1 = BUILTIN + 2;
  static final int THIS_CLASS_1 = BUILTIN + 3;
  static final int FUNCTOR_3 = BUILTIN + 4;
  static final int NAME_3 = BUILTIN + 5;

  static final int ABORT_0 = BUILTIN + 6;
  static final int RESTART_0 = BUILTIN + 7;
  static final int SHELL_1 = BUILTIN + 8;
  static final int RUNTIME_2 = BUILTIN + 9;
  static final int GLOBAL_STACK_2 = BUILTIN + 10;
  static final int LOCAL_STACK_2 = BUILTIN + 11;
  static final int TRAIL_2 = BUILTIN + 12;
  static final int CODE_2 = BUILTIN + 13;
  static final int STRINGS_2 = BUILTIN + 14;
  static final int SYMBOLS_2 = BUILTIN + 15;
  static final int HTABLE_2 = BUILTIN + 16;
  static final int LIST_ASM_3 = BUILTIN + 17;
  static final int SERIALIZE_1 = BUILTIN + 18;
  static final int BB_LIST_1 = BUILTIN + 19;
  static final int STOP_0 = BUILTIN + 20;
  static final int PROFILE_0 = BUILTIN + 21;

  static final int GC_2 = BUILTIN + 22;
  static final int APPLY = BUILTIN + 23; // not really a builtin with arity
  static final int LAST_BUILTIN = BUILTIN + 24;
  static final int NOP = LAST_BUILTIN + 1;

  /**
  * "Macros" start here
  */

  private static final int INSTR_SIZE = 32;
  private static final int ARITYBITS = 8;
  private static final int REGBITS = 11;
  private static final int ARGBITS = 11;

 
  /** Maximim value for instruction codes, both WAM and Builtin.*/
  final static int MAXOP = 256;
  final static int MAXARITY = 1 << ARITYBITS;

  private static final int OPBITS = (INSTR_SIZE - REGBITS - ARGBITS);
  // if OPBITS > 10 it will be slow on SPARCs 

  static final int MAXREG = (1 << (ARGBITS) - 1);
  /* DATA AREA FORMATS & INSTRUCTION FORMATS */

  private static final int TAGBITS = 2;

  public final static int MAXINT=(1<<(32-TAGBITS-2))-1;
    
  // static final int getTagBits() {return TAGBITS;}
  /**********************LOW-LEVEL TERM OPERATIONS*****************/
  // Term representation:
  // int <INTTAG><integer>
  // fun <FUNTAG><Symbol><Arity>
  // var <VARTAG><heap reference> : this is just a heap reference as VARTAG=0
  // |TAG|SYMNO|ARITY| or |TAG|DATA|

  private static final int LBITS = TAGBITS;
  private static final int RBITS = ARITYBITS;
  private static final int MBITS = (INSTR_SIZE - LBITS - RBITS);

  private static final int RMASK = ((1 << RBITS) - 1);
  private static final int LMASK = (~0 << (MBITS + RBITS));
  private static final int MMASK = (~LMASK & ~RMASK);

  private static final int LGET(int W) { return W >>> (MBITS + RBITS); }
  private static final int LPUT(int W, int val) { return ((W & ~LMASK) | (val << (MBITS + RBITS))); }

  private static final int MGET(int W) { return (W) << LBITS >>> (LBITS + RBITS); }
  private static final int MPUT(int W, int val) { return ((W & ~MMASK) | (val << (LBITS + RBITS) >>> LBITS)); }

  private static final int RGET(int W) { return W & RMASK; }
  private static final int RPUT(int W, int val) { return ((W & ~RMASK) | val); }

  /*******************INTERFACE TERM OPERATIONS ******************/

  private static final int PUTTAG(int W, int val) { return LPUT(W, val); }

  static final int PUTSYMNO(int W, int val) { return MPUT(W, val); }
  static final int GETSYMNO(int W) { return MGET(W); }

  static final int PUTARITY(int W, int val) { return RPUT(W, val); }
  public static final int GETARITY(int W) { return RGET(W); }

  /* 
   VAR tags are assumed 0 - otherwise it can all change
   */
  static private final int TAGMASK = LMASK;

  static private final int VARTAG = PUTTAG(0, 0); // assumed 0
  static private final int FUNTAG = PUTTAG(0, 1);
  static private final int INTTAG = PUTTAG(0, 2);

  //static private final int noTAG =  PUTTAG(0,3);

  /*************** HIGH-LEVEL TERM OPERATIONS************************/
  public static final boolean isNONVAR(int hRef) { return (hRef & TAGMASK) != VARTAG; }
  // {return isIDENTIFIER(hRef) || isINTEGER(hRef);}
  public static final boolean isVAR(int hRef) { return (hRef & TAGMASK) == VARTAG; }
  public static final boolean isINTEGER(int hRef) { return (hRef & TAGMASK) == INTTAG; }
  public static final boolean isIDENTIFIER(int hRef) { return (hRef & TAGMASK) == FUNTAG; }

  public static final boolean isSYMCONST(int hRef) { return isIDENTIFIER(hRef) && GETARITY(hRef) == 0; }
  static final boolean isCOMPOUND(int hRef) { return isIDENTIFIER(hRef) && GETARITY(hRef) != 0; }
  static final boolean isATOMIC(int hRef) { return isINTEGER(hRef) || isSYMCONST(hRef); }

  //************** Packing/unpacking integer terms.
  // FIXED UGLY BUG with negative numbers confused with compound terms - uses unsigned right shift >>> PT
  public static final int INPUT_INT(int val) { return ((val << TAGBITS) >>> TAGBITS) | INTTAG; }
  public static final int OUTPUT_INT(int val) { return (val << TAGBITS) >> TAGBITS; }

  static final int addArity(int symno, int arity) {
    return PUTARITY(PUTSYMNO(FUNTAG, symno), arity);
  }

  /*************** LOW-LEVEL INSTRUCTION OPERATIONS *****************/

  private static final int LCBITS = REGBITS;
  private static final int MCBITS = ARGBITS;
  private static final int RCBITS = OPBITS;

  private static final int RCMASK = ((1 << RCBITS) - 1);
  private static final int LCMASK = (~0 << (MCBITS + RCBITS));
  private static final int MCMASK = (~LCMASK & ~RCMASK);

  static final int LCGET(int W) { return (W >>> (MCBITS + RCBITS)); }
  static final int LCPUT(int W, int val) { return (W & ~LCMASK) | (val) << (MCBITS + RCBITS); }

  static final int MCGET(int W) { return (W << (LCBITS) >>> (LCBITS + RCBITS)); }
  static final int MCPUT(int W, int val) { return ((W & ~MCMASK) | (val << (LCBITS + RCBITS) >>> LCBITS)); }

  static final int RCGET(int W) { return (W & RCMASK); }
  static final int RCPUT(int W, int val) { return ((W & ~RCMASK) | val); }

  /**
     shows content of a cell
   */

  static String showCell(int val) {
    String s;
    if (isVAR(val)) s = "_" + val;
    else if (isINTEGER(val)) s = "" + OUTPUT_INT(val);
    else if (isIDENTIFIER(val)) s = "<" + GETSYMNO(val) + ">/" + GETARITY(val);
    else s = "unknown?" + LGET(val) + "#" + val;
    return s;
  }


  /**
    shows some low level data
   */
  void showLowLevel() {/*
    if(true) {
      int f;
      try {
        Prolog.dump(
          "\nLow level data\n"+
          "\nminus one:"+INPUT_INT(-1)+
          " compound -1:"+isCOMPOUND(INPUT_INT(-1))+
          "\nplus one:"+INPUT_INT(1)+
          " compound 1:"+isCOMPOUND(INPUT_INT(1))+
          "\nminus 2:"+INPUT_INT(-2)+
          " compound -2000:"+isCOMPOUND(INPUT_INT(-2000))+
          //"\nf/1:"+(f=inputTerm("f",1))+
          //" compound f/1:"+isCOMPOUND(f)+" arity:"+GETARITY(f)+
          "\nFUNTAG:"+FUNTAG+" INTTAG:"+INTTAG+
          "\n"
          );
      }
      catch(Exception ignore){}
    }
   */
  }
}

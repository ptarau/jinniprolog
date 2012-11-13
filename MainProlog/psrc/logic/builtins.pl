/*
% ---------- ADD BUILTINS: <here> and in ru.c -----------------------
*/
n_binline(50).                           % first in_body->INLINE
n_barith(X1):-n_binline(X),X1 is X+11.    % first arith in_body->ARITH
n_bbuiltin(X1):-n_barith(X),X1 is X+62.   % first in_head builtin->BUILTIN

bbuiltin(fail(_),0,in_body).
bbuiltin(cwrite(_,_),1,in_body).
bbuiltin(nl(_),2,in_body).
bbuiltin(var(_,_),3,in_body).
bbuiltin(nonvar(_,_),4,in_body).
bbuiltin(integer(_,_),5,in_body).
bbuiltin(atomic(_,_),6,in_body).

bbuiltin(is_compiled(_,_),7,in_body).
bbuiltin(return(_,_),8,in_body).

bbuiltin(seen(_),9,in_body).
bbuiltin(told(_),10,in_body).

bbuiltin(+(_,_,_,_),arith(0,1),in_body).
bbuiltin(-(_,_,_,_),arith(1,1),in_body).
bbuiltin(*(_,_,_,_),arith(2,1),in_body).
bbuiltin('//'(_,_,_,_),arith(3,1),in_body).
bbuiltin(mod(_,_,_,_),arith(4,1),in_body).
bbuiltin(random(_,_),arith(5,1),in_body).
bbuiltin(get0(_,_),arith(6,1),in_body).

bbuiltin(put(_,_),arith(7,0),in_body).
bbuiltin(<(_,_,_),arith(8,0),in_body).
bbuiltin(>(_,_,_),arith(9,0),in_body).
bbuiltin(=<(_,_,_),arith(10,0),in_body).
bbuiltin(>=(_,_,_),arith(11,0),in_body).
bbuiltin(=:=(_,_,_),arith(12,0),in_body).
bbuiltin(=\=(_,_,_),arith(13,0),in_body).

bbuiltin('<<'(_,_,_,_),arith(14,1),in_body).
bbuiltin('>>'(_,_,_,_),arith(15,1),in_body).
bbuiltin('/\'(_,_,_,_),arith(16,1),in_body).
bbuiltin('\/'(_,_,_,_),arith(17,1),in_body).
bbuiltin('#'(_,_,_,_),arith(18,1),in_body).
bbuiltin('\'(_,_,_,_),arith(19,1),in_body).

bbuiltin('compare0'(_,_,_,_),arith(20,1),in_body).
bbuiltin(arg(_,_,_,_),arith(21,1),in_body).

bbuiltin(def(_,_,_,_),arith(22,0),in_body).
bbuiltin(set(_,_,_,_),arith(23,0),in_body).
bbuiltin(val(_,_,_,_),arith(24,1),in_body).
bbuiltin(rm(_,_,_),arith(25,0),in_body).

bbuiltin(namecat(_,_,_,_,_),arith(26,1),in_body).
bbuiltin(copy_term(_,_,_),arith(27,1),in_body).
bbuiltin(new_code(_,_,_),arith(28,1),in_body).

bbuiltin(seeing(_,_),arith(29,1),in_body).
bbuiltin(telling(_,_),arith(30,1),in_body).
bbuiltin(see_or_fail(_,_),arith(31,0),in_body).
bbuiltin(tell_or_fail(_,_),arith(32,0),in_body).

bbuiltin(add_instr(_,_,_,_,_,_),arith(33,0),in_body).
bbuiltin(det_append(_,_,_,_),arith(34,1),in_body).

bbuiltin(create_engine(_,_,_,_),arith(35,1),in_body).
bbuiltin(engine_get(_,_,_),arith(36,1),in_body).
bbuiltin(engine_stop(_,_),arith(37,0),in_body).
bbuiltin(new_fluent(_,_,_,_),arith(38,1),in_body).

bbuiltin(sread0(_,_,_,_,_),arith(39,1),in_body).
bbuiltin(swrite(_,_,_),arith(40,1),in_body).

bbuiltin(new_java_class(_Name,_Cls,_),arith(41,1),in_body).
bbuiltin(new_java_object(_Cls,_Args,_Obj,_),arith(42,1),in_body).
bbuiltin(invoke_java_method(_Cls,_Obj,_Meth,_Args,_Ans,_),arith(43,1),in_body).
bbuiltin(delete_java_class(_Cls,_YesNo,_),arith(44,1),in_body).
bbuiltin(delete_java_object(_Obj,_YesNo,_),arith(45,1),in_body).
bbuiltin(get_java_field_handle(_O,_FN,_FV,_),arith(46,1),in_body).

bbuiltin(run_bg(_,_,_),arith(47,1),in_body).

bbuiltin(current_engine(_Handle,_),arith(48,1),in_body).
bbuiltin(to_engine(_Engine,_Term,_),arith(49,0),in_body).
bbuiltin(from_engine(_Term,_),arith(50,1),in_body).

% generic Function,String->String call
bbuiltin(jcall(_F,_I,_E,_),arith(51,1),in_body).

bbuiltin(queue_create(_Q,_),arith(52,1),in_body).
bbuiltin(queue_size(_Q,_Size,_),arith(53,1),in_body).
bbuiltin(queue_op(_Q,_Op,_I,_E,_),arith(54,1),in_body).
bbuiltin(queue_destroy(_Q,_),arith(55,0),in_body).
bbuiltin(queue_add(_Q,_E,_),arith(56,0),in_body).
bbuiltin(queue_pop(_Q,_E,_),arith(57,1),in_body).
bbuiltin(queue_push(_Q,_E,_),arith(58,0),in_body).
bbuiltin(queue_update_at(_Q,_I,_E,_),arith(59,0),in_body).
bbuiltin(queue_list(_Q,_Xs,_),arith(60,1),in_body).

bbuiltin(change_arg(_I,_T,_A,_),arith(61,0),in_body).

bbuiltin('$demo'(_),0,in_head).
bbuiltin('$demo'(_,_),1,in_head).

bbuiltin(class_name(_,_),2,in_head).
bbuiltin(this_class(_,_),3,in_head).

bbuiltin(functor(_,_,_,_),4,in_head).
bbuiltin(name(_,_,_,_),5,in_head).

bbuiltin(abort(_),6,in_head).
bbuiltin(restart(_),7,in_head).

bbuiltin(shell(_,_),8,in_head).
bbuiltin(runtime(_,_,_),9,in_head).
bbuiltin(global_stack(_,_,_),10,in_head).
bbuiltin(local_stack(_,_,_),11,in_head).
bbuiltin(trail(_,_,_),12,in_head).
bbuiltin(code(_,_,_),13,in_head).
bbuiltin(strings(_,_,_),14,in_head).

bbuiltin(symbols(_,_,_),15,in_head).
bbuiltin(htable(_,_,_),16,in_head).
bbuiltin(list_asm(_,_,_,_),17,in_head).
bbuiltin(serialize(_,_),18,in_head).
bbuiltin(bb_list(_,_),19,in_head).
bbuiltin(stop(_),20,in_head).
bbuiltin(profile(_),21,in_head).
bbuiltin(gc(_,_,_),22,in_head). % one more arg than it has
%bbuiltin(apply(_,_),23,in_head).

cutp(X):-atom_codes(X,"$cut").

bin_bbuiltin(('!'(X,Cont):-'$demo'(Cont))):-cutp(X).
bin_bbuiltin(C):-  
	bbuiltin(B,_,Where),
	bu_body(Where,B,C).

bu_body(in_head,B,(B:-'$demo'(Cont))):-
	functor(B,_,N),arg(N,B,Cont).
bu_body(in_body,B,(B:-B)).

math_op2(pow).
math_op2(log).
math_op2(max).
math_op2(min).
math_op2(('/')).
math_op2(compute).

math_op1(exp).
math_op1(log).
math_op1(sqrt).
math_op1(abs).
math_op1(ceil).
math_op1(sin).
math_op1(cos).
math_op1(tan).
math_op1(asin).
math_op1(acos).
math_op1(atan).
math_op1(floor).
math_op1(round).
math_op1(integer).


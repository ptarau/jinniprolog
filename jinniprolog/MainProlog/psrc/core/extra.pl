/* backtrackable fields */
uput(Key,Val):-
  current_engine_object(O),
  invoke_java_method(O,uput(Key,Val),_).

uget(Key,Val):-
  current_engine_object(O),
  invoke_java_method(O,uget(Key),Val),
  Val\=='$null'.
  
add_undoable(JavaObject):-
  current_engine_object(O),
  invoke_java_method(O,add_undoable(JavaObject),_).

% Continuation manipulations

pop_cc:-
	pop('$cont',Cont),
	bcall(Cont),
	fail.
pop_cc.

call_cc:-
	stack('$cont',[Cont|_]),
	bcall(Cont),
	fail.
call_cc.

show_cc:-
	stack('$cont',Cs),
	member(Cont,Cs),
	nl,
	cc_list(Cont,L),
	member(G,L),
	write(G),nl,
	fail.
show_cc:-nl.

strip_cont(TC,T,C):-
	functor(TC,F,N1),N is N1-1,
	functor(T,F,N),
	arg(N1,TC,C),
	copy_args(1,N,T,TC).

cc_list(Cont,Cont):-var(Cont),!.
cc_list(true,[true]):-!.
cc_list(fail(_),[fail]):-!.
cc_list(T,[G|Gs]):-
	strip_cont(T,G,Cont),
	cc_list(Cont,Gs).


/* Hilog call */

'@'(As,Conj):-nonvar(As),As=[_|_],!,call_by_association(As,Conj).
'@'(P,Conj):-hilog_call(P,Conj).

call_by_association(As,Conj):-
  nonvar(As),
  findall(P,association_to_closure(As,P),Ps),
  Ps=[P],
  !,
  hilog_call(P,Conj).
call_by_association(As,Conj):-
  errmes(ambiguous_attributes(As),cannot_find_unique_closure_for(Conj)).

association_to_closure([],_).
association_to_closure([A|As],C):-
  closure_attribute(C,A),
  association_to_closure(As,C).

closure_attribute(Closure,A):-db_clause(association_db,A,Closure).

add_association(Closure,A):-db_assert(association_db,(A:-Closure)).

add_associations(Closure,As):-map(add_association(Closure),As).

list_associations:-closure_attribute(Closure,A),println(A=>Closure),fail;true.

hilog_call(P,Conj):-
  nonvar(P),
  !,
  P=..[F|As],conj2list(Conj,Bs),
  det_append(As,Bs,Xs),
  G=..[F|Xs],
  metacall(G).
hilog_call(P,Conj):-
  errmes(unbound_hilog_call(P),on(Conj)).

conj2list(Conj,[A|Xs]):-nonvar(Conj),Conj=(A,Bs),!,conj2list(Bs,Xs).
conj2list(A,[A]).


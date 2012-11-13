idb_assert(C):-this_db(D),idb_assert(D,C).
idb_asserta(C):-this_db(D),idb_asserta(D,C).
idb_clause(H,B):-this_db(D),idb_clause(D,H,B).
idb_retract(H):-this_db(D),idb_retract(D,H).
idb_retractall(H):-this_db(D),idb_retractall(D,H).
idb_clean:-this_db(D),idb_clean(D).
idb_is_dynamic(H):-this_db(D),idb_is_dynamic(D,H).

idb_assert(D,C):-idb_assert(D,1,C).
idb_asserta(D,C):-idb_asserta(D,1,C).
idb_clause(D,H,B):-idb_clause(D,1,H,B).
idb_retract(D,H):-idb_retract(D,1,H).
idb_retractall(D,H):-idb_retractall(D,1,H).
idb_clean(D):-idb_clean(1,D).
idb_is_dynamic(D,H):-idb_is_dynamic(D,1,H).

idb_assert(D,I,C):-
  ensure_clause(C,(H:-B)),
  idb_assert0(D,I,H,B).

idb_assert0(D,_,H,B):-db_is_dynamic(D,H),!,
  db_assert(D,(H:-B)).
idb_assert0(D,I,H,B):-
  arg(I,H,A),functor(A,G,M),
  !,
  db_assert(G,(H:-'$indexed'(D,I,M,B))),
  db_assert('$args',(G:-'$arg'(D,I,M,H))).
idb_assert0(D,_,H,B):-
  db_assert(D,(H:-B)).

idb_asserta(D,I,C):-
  ensure_clause(C,(H:-B)),
  idb_asserta0(D,I,H,B).

idb_asserta0(D,_,H,B):-db_is_dynamic(D,H),!,
  db_asserta(D,(H:-B)).
idb_asserta0(D,I,H,B):-
  arg(I,H,A),functor(A,G,M),
  !,
  db_asserta(G,(H:-'$indexed'(D,I,M,B))),
  db_asserta('$args',(G:-'$arg'(D,I,M,H))).
idb_asserta0(D,_,H,B):-
  db_asserta(D,(H:-B)).

idb_retract(D,I,H):-
  nonvar(H),
  !,
  arg(I,H,A),functor(A,G,M),
  db_retract(G,(H:-'$indexed'(D,I,M,_))),
  once(db_retract('$args',(G:-'$arg'(D,I,M,H)))).
idb_retract(D,I,H):-
  var(H),
  !,
  db_retract('$args',(G:-'$arg'(D,I,M,H))),
  arg(I,H,A),functor(A,G,M),
  once(db_retract(G,(H:-'$indexed'(D,I,M,_)))).
idb_retract(D,_I,H):-
  db_is_dynamic(D,H),
  db_retract(D,H).
  
idb_retractall(D,I,H):-
  idb_retract(D,I,H),
  fail.
idb_retractall(_D,_I,_H).

idb_clean(I,D):-idb_retractall(D,I,_).

idb_is_dynamic(D,_,H):-db_is_dynamic(D,H),!.
idb_is_dynamic(D,I,H):-not_idb_is_dynamic(D,I,H),!,fail.
idb_is_dynamic(_D,_I,_H).

not_idb_is_dynamic(D,I,H):-idb_clause(D,I,H,_),!,fail.
not_idb_is_dynamic(_D,_I,_H).
  
idb_clause(D,I,H,B):-
  nonvar(H),
  arg(I,H,A),
  functor(A,G,M),
  !,
  db_clause(G,H,'$indexed'(D,I,M,B)).
idb_clause(D,I,H,B):-
  db_clause('$args',G,'$arg'(D,I,M,H)),
  db_clause(G,H,'$indexed'(D,I,M,B)).
idb_clause(D,_I,H,B):-
  db_is_dynamic(D,H),
  db_clause(D,H,B).
   
'$indexed'(_D,_I,_M,_B):-fail.
'$arg'(_D,_I,_M,_B):-fail.
  
ensure_clause(C,_):-var(C),!,errmes(clause_expected,C).
ensure_clause((H:-B),C):-!,C=(H:-B).
ensure_clause(H,(H:-true)).


% metainterpreter

idb_call(Body):-this_db(Db),idb_call(Db,Body).

idb_call(Db,Body):-idb_body(Db,Body).

idb_body(Db,Body):-var(Body),!,errmes(bad_metacall(Db),var(Body)).
idb_body(Db,Body) :-
	idb_body(Db,Body, AfterCut, HadCut),
	( HadCut = yes,
		!,
		idb_body(Db,AfterCut)
	;   HadCut = no
	).

idb_body(_Db,(!,AfterCut), AfterCut, yes) :- !.
idb_body(Db,(Goal,Body), AfterCut, HadCut) :- !,
	idb_goal(Db,Goal),
	idb_body(Db,Body, AfterCut, HadCut).
idb_body(_Db,!, true, yes).
idb_body(Db,(Disj1;_), AfterCut, HadCut) :-
	idb_body(Db,Disj1, AfterCut, HadCut).
idb_body(Db,(_;Disj2), AfterCut, HadCut) :- !,
	idb_body(Db,Disj2, AfterCut, HadCut).
idb_body(Db,Goal, true, no) :-
	idb_goal(Db,Goal).

idb_goal(_Db,Goal) :-
	is_compiled(Goal), % <--- check for a compiled predicate
	!,
	%println('calling compiled'(Goal)),
	Goal.
idb_goal(Db,Goal) :-
	idb_is_dynamic(Db,Goal),
  !,
	idb_clause(Db,Goal, Body),	% <--- assume anything else is interpreted
	idb_body(Db,Body, AfterCut, HadCut),
	(	HadCut = yes,
		!,
		idb_body(Db,AfterCut)
	;	HadCut = no
	).
idb_goal(Db,Undef):-
  idb_undefined(Db,Undef).
  
idb_undefined(Db,Undef):-
  errmes(undefined_predicate_in_call(Db),Undef).

% file operations


% FILE RELATED API AND LISTINGS
idb_consult(File,Db):-
  foreach(
    clause_of(File,C),
    idb_consult_action(C,Db)
  ).

idb_consult_action(':-'(G),Db):-if(G=[F],idb_consult(F,Db),once(idb_call(Db,G))).
idb_consult_action(':-'(H,B),Db):-idb_assert(Db,':-'(H,B),_).

idb_reconsult(File,Db):-
  idb_clean(Db),
  idb_consult(File,Db).

idb_save(Db,F):-
  telling(X),
  tell(F),
  idb_listing(Db),
  told,
  tell(X).
  
idb_listing:-idb_listing(_).

idb_listing(Db):-
  idb_listing(Db,_).
  
idb_listing(DbName,F/N):-
  (functor(H,F,N)->true;true),
  idb_clause(DbName,H,B),
  pp_clause((DbName:H:-B)),
  fail.
idb_listing(_,_).  

itest:-
  idb_assert(a(1)),
  idb_assert(a(2)),
  idb_assert((b(X):-a(X))),
  foreach(
    idb_call(b(X)),
    println(X)
  ),
  idb_listing,
  idb_retractall(_).
  
  
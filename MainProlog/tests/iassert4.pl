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
  idb_assert0(db_assert,D,I,H,B).

idb_asserta(D,I,C):-
  ensure_clause(C,(H:-B)),
  idb_asserta0(db_asserta,D,I,H,B).

idb_assert0(Op,D,_,H,B):-db_is_dynamic(D,H),!,
  aop_call(Op,D,(H:-B)).
idb_assert0(Op,D,I,H,B):-
  functor(H,F,N),I>0,I<=N,arg(I,H,A),functor(A,G,M),
  !,
  aop_call(Op,G,(H:-'$indexed'(D,I,M,B))),
  % as N>0 we can assume F<>H
  aop_call(Op,D,(F:-'$arg'(N,I,M,G))).
idb_assert0(Op,D,_,H,B):-
  aop_call(Op,D,(H:-B)).

aop_call(db_assert,D,C):-db_assert(D,C).
aop_call(db_asserta,D,C):-db_asserta(D,C).

idb_retract(D,I,H):-
  functor(H,F,N),I>0,I<=N,arg(I,H,A),
  arg(I,H,A),functor(A,G,M),
  !,
  db_retract(G,(H:-'$indexed'(D,I,M,_))),
  once(db_retract(D,(F:-'$arg'(N,I,M,G)))).
idb_retract(D,I,H):-
  var(H),
  !,
  db_retract(D,(F:-'$arg'(N,I,M,G))),
  functor(H,F,N),
  arg(I,H,A),functor(A,G,M),
  once(db_retract(G,(H:-'$indexed'(D,I,M,_B)))).
idb_retract(D,_I,H):-
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
  db_clause(D,F,'$arg'(N,I,M,G)),
  functor(H,F,N),
  once(db_clause(G,H,'$indexed'(D,I,M,B))).
idb_clause(D,_I,H,B):-
  db_clause(D,H,B),
  functor(H,F,_),
  functor(B,G,_),
  G\=='$indexed',
  G\=='$arg',
  F\=='$field'.
   
'$indexed'(_D,_I,_M,_B):-fail.
'$arg'(_N,_I,_M,_G):-fail.
  
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

% idb_consult_action(C,Db):-write(trace(Db)),put(32),println(C),fail.
idb_consult_action(':-'(G),Db):-if(G=[F],idb_consult(F,Db),once(idb_call(Db,G))).
idb_consult_action(':-'(H,B),Db):-idb_assert(Db,':-'(H,B)).

idb_reconsult(File,Db):-
  idb_clean(Db),
  idb_consult(File,Db).

idb_save(Db,F):-
  telling(X),
  tell(F),
  idb_listing(Db),
  told,
  tell(X).
  
idb_listing:-this_db(Db),idb_listing(Db).

idb_listing(Db):-
  idb_listing(Db,_).
  
idb_listing(DbName,F/N):-
  [Cm]="%",
  put(Cm),println(database:DbName),nl,
  (functor(H,F,N)->true;true),
  idb_clause(DbName,H,B),
  pp_clause((H:-B)),
  fail.
idb_listing(_,_).  


% this DB
idb_consult(File):-
  this_db(Db),
  idb_consult(File,Db).
  
idb_reconsult(File):-
  this_db(Db),
  idb_reconsult(File,Db).


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
  

mcat(K1,K2,K12):-val(K1,K2,R),!,R=K12.
mcat(K1,K2,K12):-namecat(K1,'_X_',K2,K12),def(K1,K2,K12).


% ground query nonground clause ???

ixtest:-
  iassertx(d,a(m,s),f(1)),
  iassertx(d,a(n),f(2)),
  iassertx(d,a(_,q),f(3)),
  iassertx(d,a(_),f(4)),
  iassertx(d,a(n),f(5)),
  iassertx(d,a(p),f(6)),
  iassertx(d,a(p,q),f(7)),
  db_listing(d),
  db_listing(d_X_a).
  
ixtest1:-
  iassertx(d,a(m,m1),1),
  iassertx(d,a(m),2),
  iassertx(d,a(_),3),
  iassertx(d,a(k),4),
  % foreach(iclausex(d,H,B),println(d:(H:-B))),
  foreach(iclausex(d,a(k),B),println(a:X+B)),
  println('LISTINGS'),
  % db_listing(d),
  db_listing(d_X_a).
  
  

iassertx(C):-this_db(D),iassertx(D,C).  

iassertx(D,C):-ensure_clause(C,(H:-B)),iassertx(D,H,B).

iassertx(D,H,B):-iassertx0(D,H,B),db_assert(D,(H:-B)).

iassertx0(D,H,B):-head2i(D,H,PH,DF,FA,N),!,iassertx_once(D,DF,FA,arity(N)),db_assert(D,(PH:-B)).
iassertx0(D,H,B):-foreach(head2n(D,H,PH),db_assert(D,(PH:-B))).

iassertx_once(_D,DF,FA,arity(N)):-db_clause(DF,FA,arity(N)),!.
iassertx_once(D,DF,FA,arity(N)):-db_clause(DF,FB,arity(N)),FB\==FA,!,
  functor(XPH,FB,N),arg(1,XPH,H),
  functor(PH,FA,N),arg(1,PH,H),
  ( db_clause(D,XPH,B),
    % println(inheriting(FB=>FA)-H),
    arg(1,H,A),
    var(A),
    db_assert(D,(PH:-B)),
    fail  
  ; true
  ),
  db_assert(DF,(FA:-arity(N))).
iassertx_once(_D,DF,FA,ARITY):-
  db_assert(DF,(FA:-ARITY)).

iclausex(H,B):-this_db(D),iclausex(D,H,B).

iclausex(D,H,B):-println(here=H+B),head2i(D,H,PH,_DF,_FA,_N),println(there=PH),!,db_clause(D,PH,B).
iclausex(D,H,B):-db_clause(D,H,B).
  
head2n(D,H,PH):-
  functor(H,F,N),
  mcat(D,F,DF),
  db_clause(DF,FA,arity(N)),
  functor(PH,FA,1),
  arg(1,PH,H).

head2i(D,H,PH,DF,FA,N):-
  functor(H,F,N),
  arg(1,H,A),
  functor(A,G,_M),
  mcat(F,G,FA),
  functor(PH,FA,1),
  arg(1,PH,H),
  mcat(D,F,DF).
  
i2head(HA,H):-arg(1,HA,H).

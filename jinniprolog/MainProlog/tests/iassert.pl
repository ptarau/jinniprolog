idb_assert(C):-this_db(D),idb_assert(D,C).
idb_asserta(C):-this_db(D),idb_asserta(D,C).
idb_clause(H,B):-this_db(D),idb_clause(D,H,B).
idb_retract(H):-this_db(D),idb_retract(D,H).
idb_retractall(H):-this_db(D),idb_retractall(D,H).
idb_clean:-this_db(D),idb_clean(D).
idb_is_dynamic(H):-this_db(D),idb_is_dynamic(D,H).

aop_call(db_assert,D,C):-db_assert(D,C).
aop_call(db_asserta,D,C):-db_asserta(D,C).

ensure_clause(C,_):-var(C),!,errmes(clause_expected,C).
ensure_clause((H:-B),C):-!,C=(H:-B).
ensure_clause(H,(H:-true)).

% metainterpreter

idb_call(Body):-this_db(Db),idb_call(Db,Body).

idb_call(Db,Body):-idb_body(Db,Body).

% idb_body(Db,Body):-println(entering_body_in(Db)=Body),fail.
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
  idb_assert(d,a(m,s),f(1)),
  idb_assert(d,a(n),f(2)),
  idb_assert(d,a(_,q),f(3)),
  idb_assert(d,a(_),f(4)),
  idb_assert(d,a(n),f(5)),
  idb_assert(d,a(p),f(6)),
  idb_assert(d,a(p,q),f(7)),
  db_listing(d),
  db_listing(d_X_a).
  
ixtest1:-
  idb_assert(d,a(m,m1),1),
  idb_assert(d,a(m),2),
  idb_assert(d,a(_),3),
  idb_assert(d,a(k),4),
  % foreach(idb_clause(d,H,B),println(d:(H:-B))),
  foreach(idb_clause(d,a(X),B),println(a:X+B)),
  println('LISTINGS'),
  % db_listing(d),
  db_listing(d_X_a).

ixtest2:-
  idb_consult('temp'),
  idb_listing,
  % idb_call(go),
  true.  
  
inrev:-
  idb_consult('../bin/progs/nrev.pl'),
  idb_call(small).  


ibug:-
  idb_assert(d,go(a)),
  idb_listing(d),
  idb_assert(d,go),
  idb_listing(d).
  
% idb_assert(C):-this_db(D),idb_assert(D,C).  

idb_assert(D,C):-ensure_clause(C,(H:-B)),idb_assert(D,H,B).

idb_assert(D,H,B):-
  % println(trying(H,B)),
  idb_assert0(D,H,B),
  % println(here(D,H)),
  db_assert(D,(H:-B)).
  % println(there(D,H)).

idb_assert0(D,H,B):-head2i(D,H,PH,DF,FA,N),!,idb_assert_once(D,DF,FA,arity(N)),db_assert(D,(PH:-B)).
idb_assert0(D,H,B):-foreach(head2n(D,H,PH),db_assert(D,(PH:-B))).

idb_assert_once(_D,DF,FA,arity(N)):-db_clause(DF,FA,arity(N)),!.
idb_assert_once(D,DF,FA,arity(N)):-db_clause(DF,FB,arity(N)),FB\==FA,!,
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
idb_assert_once(_D,DF,FA,ARITY):-
  db_assert(DF,(FA:-ARITY)).

% idb_clause(H,B):-this_db(D),idb_clause(D,H,B).

% idb_clause(D,H,B):-println(D:H=>B),fail.
idb_clause(D,H,B):-head2i(D,H,PH,_DF,_FA,_N),db_is_dynamic(D,PH),!,
  % println(here=H+PH),
  db_clause(D,PH,B).
idb_clause(D,H,B):-db_clause(D,H,B).


idb_retract(D,H):-idb_retract(D,H,_B).

idb_retract(D,H,B):-head2i(D,H,PH,_DF,_FA,_N),db_is_dynamic(D,PH),!,db_retract(D,PH,B).
idb_retract(D,H,B):-foreach(head2n(D,H,PH),db_retract(D,PH,B)),fail.
idb_retract(D,H,B):- 
  db_retract(D,H,B).

idb_retractall(D,H):-
  idb_retractall(D,H,_).
  
idb_retractall(D,H,B):-
  idb_retract(D,H,B),
  fail.
idb_retractall(_D,_H,_B).

idb_clean(D):-idb_retractall(D,_).

idb_is_dynamic(D,H):-db_is_dynamic(D,H).
  
head2n(D,H,PH):-
  functor(H,F,N),
  N>0,
  mcat(D,F,DF),
  db_clause(DF,FA,arity(N)),
  functor(PH,FA,1),
  arg(1,PH,H).

head2i(D,H,PH,DF,FA,N):-
  functor(H,F,N),
  N>0,
  arg(1,H,A),
  functor(A,G,_M),
  mcat(F,G,FA),
  functor(PH,FA,1),
  arg(1,PH,H),
  mcat(D,F,DF).
  
i2head(HA,H):-arg(1,HA,H).

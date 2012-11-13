% note that the Db associated to This can be used as O of type Prolog.class

this_db(Db):-get_instance_id(Db).

set_db(Db):-set_instance_id(Db).

get_class_db(0).

/* stacks - derived */

push(S,X):-this_db(O),push(O,S,X).
pop(S,X):-this_db(O),pop(O,S,X).
stack(S,X):-this_db(O),stack(O,S,X).

/* queues - derived */

enq(S,X):-this_db(O),enq(O,S,X).
deq(S,X):-this_db(O),deq(O,S,X).
qpush(S,X):-this_db(O),qpush(O,S,X).
queue(S,X):-this_db(O),queue(O,S,X).
% qmembi(S,X,I):-this_db(O),qmembi(O,S,X,I).
qmembc(S,C):-this_db(O),qmembc(O,S,C).
% qdeli(S,I):-this_db(O),qdeli(O,S,I).

% clause: looks up current instance, class as well as debug_clauses brought by debug(File)
  
db_clause(O,H,B):-
  var(H),
  !,
  db_get_one_clause(O,H,B).
db_clause(O,H,B):-
  db_is_dynamic(O,H),
  !,
  db_get_one_clause(O,H,B).
db_clause(_,H,B):-
  get_class_db(ClassDb),
  db_is_dynamic(ClassDb,H),
  !,
  db_get_one_clause(ClassDb,H,B).
db_clause(_O,H,B):- 
  % _O might be something else - not a Prolog CodeSpace !!!
  this_class(O),
  call_engine(O,
    call_ifdef(debug_clause(H,B),fail)
  ).

% managing current dynamic predicates

db_get_preds(Db,FNs):-findall(FN,db_get_pred(Db,FN),FNs).

db_get_pred(Db,FN):-qmembc('$dynamic',Db,FN).

db_add_dynamic(Db,H):-def(H,Db,yes),functor(H,F,N),enq('$dynamic',Db,F/N).

db_rm_dynamic(Db,H):-functor(H,F,N),qdel('$dynamic',Db,F/N),rm(H,Db),!.
db_rm_dynamic(_,_).

db_dynamic(Db,F/N):-functor(H,F,N),db_make_dynamic(Db,H).

db_make_dynamic(Db,H):-db_check_dynamic(Db,H),!.
db_make_dynamic(Db,H):-db_add_dynamic(Db,H),!.
db_make_dynamic(Db,H):-
  errmes(error_in_adding_to(Db),bad_predicate(H)).

db_is_dynamic(O,H):-
  db_check_dynamic(O,H),
  !.
db_is_dynamic(_,H):-
  get_class_db(Db),
  db_check_dynamic(Db,H).

db_check_dynamic(Db,H):-val(H,Db,yes).

db_get_clauses(O,Cs):-db_get_clauses(O,_,Cs).

db_get_clauses(O,H,Cs):-
  findall((H:-B),db_get_one_clause(O,H,B),Cs).
  
db_get_one_clause(Db,H,B):-var(H),!,
  db_get_pred(Db,F/N),
  functor(H,F,N),
  qmembc(Db,H,(H:-B)).   
db_get_one_clause(Db,H,B):-
  qmembc(Db,H,(H:-B)).

% assert/retract operations

db_asserted(O,H):-
  nonvar(H),
  db_clause(O,H,B),
  do_body(B).

db_assert(O,C0):-db_assertz(O,C0).

db_assertz(O,C0):-
  to_clause(C0,C),
  C=(H:-_),
  db_make_dynamic(O,H),
  enq(O,H,C).
  
db_asserta(O,C0):-
  to_clause(C0,C),
  C=(H:-_),
  db_make_dynamic(O,H),
  qpush(O,H,C).
  
db_retract1(O,H):-db_retract1(O,H,_).
  
db_retract1(O,H,B):-
  C=(H:-B),
  qdel(O,H,C).

db_retract(O,H):-db_retract(O,H,_).

db_retract(O,H,B):-var(H),!,
  db_get_preds(O,Ps),
  member(F/N,Ps),
  functor(H,F,N),
  db_retract0(O,H,B).
db_retract(O,H,B):-  
  db_retract0(O,H,B).
  
db_retract0(O,H,B):-
  repeat,
  ( db_retract1(O,H,B)->true
  ; !,fail
  ).

db_retractall(O,H,B):-
  db_retractall0(O,H,B),
  fail.  
db_retractall(_,_,_).

db_retractall(O,H):-db_retractall(O,H,_).

db_retractall0(O,H,B):-
  functor(H,_,N),
  for(I,1,N),
  arg(I,H,A),
  nonvar(A),
  !,
  db_retract(O,H,B).
db_retractall0(O,H,_):-
  qrm(O,H).

db_abolish(O,F/N):-db_abolish(O,F,N).
  
db_abolish(O,F,N):-
  functor(H,F,N),
  % db_retractall(O,H),
  qrm(O,H),
  db_rm_dynamic(O,H),
  !.
db_abolish(_,_,_).  
  
db_clean(Db):-
  db_get_preds(Db,Ps),
  foreach(
    member(F/N,Ps),
    db_abolish(Db,F,N)
  ).

is_dynamic(H):-this_db(O),db_is_dynamic(O,H).

dynamic(FN):-this_db(O),db_dynamic(O,FN).

get_pred(FN):-this_db(O),db_get_pred(O,FN).

get_preds(FNs):-this_db(O),db_get_preds(O,FNs).

get_clauses(Cs):-this_db(O),db_get_clauses(O,Cs).
  
clause(H,B):-this_db(O),db_clause(O,H,B).

asserted(C):-this_db(O),db_asserted(O,C).

asserta(C):-this_db(O),db_asserta(O,C).

assert(C):-this_db(O),db_assert(O,C).

assertz(C):-assert(C).

retract1(H):-this_db(O),db_retract1(O,H).

retract(H):-this_db(O),db_retract(O,H).

retractall(H):-this_db(O),db_retractall(O,H).

abolish(F/N):-abolish(F,N).

abolish(F,N):-this_db(O),db_abolish(O,F,N).

db_clean:-
  this_db(O),
  db_clean(O).
  


let(X,Y,V):-def(X,Y,V),!.
let(X,Y,V):-set(X,Y,V).

default_val(X,Y,V,_):-val(X,Y,V),!.
default_val(X,Y,D,D):-def(X,Y,D).

def(X,A):-this_db(O),def(O,X,A).
set(X,A):-this_db(O),set(O,X,A).
val(X,A):-this_db(O),val(O,X,A).
let(X,A):-this_db(O),let(O,X,A).
rm(X):-this_db(O),rm(O,X).

% for BinProlog compatibility
bb_def(X,Y,A):-def(X,Y,A).
bb_set(X,Y,A):-set(X,Y,A).
bb_val(X,Y,A):-val(X,Y,A).
bb_let(X,Y,A):-let(X,Y,A).
bb_rm(X,Y):-rm(X,Y).

bb_def(X,A):-def(X,A).
bb_set(X,A):-set(X,A).
bb_val(X,A):-val(X,A).
bb_let(X,A):-let(X,A).
bb_rm(X):-rm(X).


/* gensysm */

gensym(Root,Symbol):-gensym_no(Root,SymNo),symcat(Root,SymNo,Symbol).

gensym_reset_no(Root,SymNo):-let(gensym,Root,SymNo).

gensym_init(Root):-gensym_reset_no(Root,0).

init_gensym(Root):-gensym_reset_no(Root,0).

gensym_no(Root,SymNo):-
        val(gensym,Root,N),!,
        N1 is N+1,
        set(gensym,Root,N1),
        SymNo=N1.
gensym_no(Root,SymNo):-
        def(gensym,Root,1),
        SymNo=1.

% generalized global variables: 2 ground keys -> 1 arbitrary term

global_get(A,B,X):-global_get0(A,B,X,_,_).

global_rm(A,B):-global_get0(A,B,_,K,Xs),!,g_trim(K,Xs).
global_rm(_,_).

g_trim(K,[]):-!,bb_rm('$global',K).
g_trim(K,Xs):-bb_set('$global',K,Xs).

global_get0(A,B,X,K,Ys):-
  g_key(A,B,X, K,T), 
  bb_val('$global',K,Xs),
  !,
  g_sel(T,Xs,Ys).
  
global_set(A,B,X):-
  nonvar(X),
  g_key(A,B,X, K,T),
  g_set_k(K,T).
  
g_set_k(K,T):-  
  bb_val('$global',K,Xs),
  !,
  g_update(T,Xs,NewXs),
  bb_set('$global',K,NewXs).
g_set_k(K,T):-
  bb_def('$global',K,[T]).

g_key(A,B,X, K,G-X):-G='$2'(A,B),term_hash(G,K0),K is -(1+(K0 mod 1999)).
 
g_update(G-X,Xs,Ys):-g_sel(G-_,Xs,NewXs),!,Ys=[G-X|NewXs].
g_update(T,Xs,[T|Xs]).

g_sel(G-X,[G1-Y|Xs],Xs):-G==G1,!,X=Y.
g_sel(T,[S|Xs],[S|Ys]):-g_sel(T,Xs,Ys).


% portable hashing - values valid after saving to persistent storage

hkey(T,K):-jcall(hkey,T,R),R>=0,K=R.

term_hash0(X,Key):-jcall(term_hash,X,Key).

term_hash(X,Key):-term_hash0(X,Key),Key>0,!.
term_hash(X,_Key):-errmes(hash_key_should_be_ground,X).

ground(X):-term_hash0(X,K),K>0.

/*
term_hash(X,Key):-atomic(X),!,hkey(X,Key).
term_hash([X|Xs],Key):-!,nonvar(Xs),lterm_hash([X|Xs],Key).
term_hash(T,Key):-
  functor(T,_,N),
  hkey(T,K0),
  term_hash1(0,N,T,K0,Key).

term_hash1(N,N,_,R1,R2):-!,R2=R1.
term_hash1(I1,N,P,R1,R3):-
  I is I1+1,
  arg(I,P,X),
  hkey(X,K),
  R2 is (R1<<4) + K,
  term_hash1(I,N,P,R2,R3). 

lterm_hash(Xs,N):-hkey([],N1),lterm_hash(Xs,N1,N2),abs(N2,N).
  
lterm_hash([],N,N).
lterm_hash([X|Xs],N1,N3):-
  hkey(X,H),
  N2 is (N1<<4)+H,
  lterm_hash(Xs,N2,N3).
*/

to_clause(X,_):-var(X),!,errmes('to clause: should be nonvar',var(X)).
to_clause(X,C):-functor(X,(':-'),_),!,C=X.
to_clause(X,C):-functor(X,('-->'),2),!,expand_term(X,C).
to_clause(X,C):-C=':-'(X,true).

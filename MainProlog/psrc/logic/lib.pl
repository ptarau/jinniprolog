% LIBRARY of basic predicates

true.

A->B :- A,!,B.

once(G):-G,!.


(A->B) ; C :- !,if(A,B,C).
';'(X,_):-X.
';'(_,X):-X.

if(A,B,_):-A,!,B.
if(_,_,C):-C.

(X,Y):-X,Y.

not(X):-X,!,fail.
not(_).

\+(X):-X,!,fail.
\+(_).

eq(X,X).
'='(X,X).

or(A,_):-A.
or(_,B):-B.

and(X,Y):-X,Y.
and(X,Y,Z):-X,Y,Z.
and(X,Y,Z,U):-X,Y,Z,U.
and(X,Y,Z,U,V):-X,Y,Z,U,V.
and(X,Y,Z,U,V,W):-X,Y,Z,U,V,W.

call(X):-X.
call(F,X):-term_append(F,args(X),G),G.
call(F,X,Y):-term_append(F,args(X,Y),G),G.
call(F,X,Y,Z):-term_append(F,args(X,Y,Z),G),G.
call(F,X,Y,Z,U):-term_append(F,args(X,Y,Z,U),G),G.
call(F,X,Y,Z,U,V):-term_append(F,args(X,Y,Z,U,V),G),G.
call(F,X,Y,Z,U,V,W):-term_append(F,args(X,Y,Z,U,V,W),G),G.
call(F,X,Y,Z,U,V,W,A):-term_append(F,args(X,Y,Z,U,V,W,A),G),G.
call(F,X,Y,Z,U,V,W,A,B):-term_append(F,args(X,Y,Z,U,V,W,A,B),G),G.
call(F,X,Y,Z,U,V,W,A,B,C):-term_append(F,args(X,Y,Z,U,V,W,A,B,C),G),G.
call(F,X,Y,Z,U,V,W,A,B,C,D):-term_append(F,args(X,Y,Z,U,V,W,A,B,C,D),G),G.
            
for(Min,Min,Max):-Min=<Max.
for(I,Min,Max):-
  Min<Max,
  Min1 is Min+1,
  for(I,Min1,Max).

argn(I,T,X):-nonvar(I),!,arg(I,T,X).
argn(I,T,X):-functor(T,_,N),for(I,1,N),arg(I,T,X).

repeat.
repeat:-repeat.

/* changed - double negation based definition breaks 
   if Then fails - this is counterintuitive 
*/

forall(When,Then):- When,once(Then),fail.
forall(_,_).

foreach(When,Then):- When,once(Then),fail.
foreach(_,_).

for_all(Goal,OtherGoal):- \+((Goal, \+(OtherGoal))).

case(X,Gs):-
  if(
    member(':'(X,G),Gs),
    G,
    fail
  ).

compare(R,X,Y):-compare0(X,Y,R).

A==B :- compare0(A,B,(=)).
A\==B :- compare0(A,B,R),'$noteq'(R).

distinct(X,Y):- X\==Y.

A @< B :- compare0(A,B,<).
A @> B :- compare0(A,B,>).
A @=< B :- compare0(A,B,R),'$lesseq'(R).
A @>= B :- compare0(A,B,R),'$gteq'(R).

'$lesseq'(<).
'$lesseq'(=).

'$gteq'(>).
'$gteq'(=).

'$noteq'(<).
'$noteq'(>).

% essential services

% compiled builtins

call_ifdef(G,_):-is_compiled(G),!,G.
call_ifdef(G,_):-is_dynamic(G),!,metacall(G).
call_ifdef(_,Other):-Other.
    
is_builtin(H):-
  nonvar(H),!,
  functor(H,F,N),N1 is N+1,
  functor(H1,F,N1),
  bbuiltin(H1,_,_).
is_builtin(H):-
  var(H),
  bbuiltin(T,_,_),
  functor(T,F,N),
  N1 is N-1,
  functor(H,F,N1).
 
ctime(T):-runtime(T,_).

bg(Goal):-bg(Goal,Thread),delete_java_object(Thread).

bg(Goal,Thread):-
  new_engine(ignore,bg_call(Goal),Engine),
  run_bg(Engine,Thread).

bg_call(Goal):-Goal,fail.
% bg_call(Goal):-catch((Goal,fail),Ex,warnmes(Ex,bg(Goal))),fail.

user_error(Mes,Obj):-errmes(Mes,Obj).

% from co.pl
errmes(Mes,Obj):-
  E=error(Mes,Obj),
	throw(E),
	fail.

warnmes(Mes,Obj):-
  E=warning(Mes,in(Obj)),
  call_ifdef(println(E),fail),
  fail.
  
% end
	
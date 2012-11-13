/* builtin new_engine(Answer,Goal,Handle) needs to have non-var input args!!!
   works with engine_get(Handle,Answer) and engine_stop(Handle)
   new_engine+get+stop actually works like any other Kernel Prolog fluent   
   works with: new_code(File,O) creating a code space object based on given file
*/

new_engine(XG,R):-this_class(O),create_engine(O,XG,R).
new_engine(X,G,R):-this_class(O),new_engine(O,X,G,R).
new_engine(O,X,G,R):-create_engine(O,(X:-G),R).

% reusese create engine - negative O handle indicates it is an existing engine
reuse_engine(XG,E):-O is 0-E,create_engine(O,XG,_R).
reuse_engine(X,G,E):-reuse_engine((X:-G),E).

/* API
 engine -> get_instance_id(E,ID) -> Instance -> Db handle
       |-> class->className
*/


get_instance_id(ID):-current_engine(O),get_instance_id(O,ID).
  
get_instance_id(O,ID):-jcall('get_instance_id',O,ID).

set_instance_id(ID):-jcall(set_instance_id,ID,_).

new_instance_id(ID):-current_engine(O),new_instance_id(O,ID).
  
new_instance_id(O,ID):-jcall('new_instance_id',O,ID).

this_engine(E):-current_engine(E).
  
call_engine(Goal):-this_class(C),call_engine(C,Goal).

call_engine(O,Goal):-call_engine(O,Goal,Goal,Goal).

call_engine(X,Goal,Answer):-
  this_class(C),
  call_engine(C,X,Goal,Answer).
  
call_engine(O,X,Goal,Answer):-
  new_engine(O,X,Goal,E),
  element_of(E,Answer).

/*  
ask_engine(G):-
  ask_engine(G,G,the(G)).
  
ask_engine(X,G,Answer):-this_class(C),ask_engine(C,X,G,Answer).

ask_engine(O,X,G,Answer):- 
  new_engine(O,X,G,Solver),
  get(Solver,R),
  R\==no,
  stop(Solver),
  Answer=R.
*/
  
/* engine operations */

get(E,X):-integer(E),!,engine_get(E,X).
get(E,_):-errmes(error_in_get,should_be_integer(E)).

stop(E):-integer(E),!,engine_stop(E).
stop(E):-errmes(error_in_stop,should_be_integer(E)).

throw(E):-return(exception(E)).
 
% could leak memory - the engine will only stop on fail
catch(Goal,Exception,OnException):-
  new_engine('$answer'(Goal),Goal,Source),
  catch_over(Source,Goal,Exception,OnException).
  
catch_over(Source,Goal,Exception,OnException):-
  element_of(Source,Answer),
  do_catch(Answer,Goal,Exception,OnException,Source).

% avoid leaks for det calls - use this in services !!!  
catch_once(Goal,Exception,OnException):-
  new_engine('$answer'(Goal),Goal,Source),
  get(Source,R),
  R\==no,
  the(Answer)=R,
  (nonvar(Answer),Answer='$answer'(_)->stop(Source);true),
  do_catch(Answer,Goal,Exception,OnException,Source).

do_catch(X,Goal,_,_,_):-var(X),!,return(unexpected_var_while_catching(Goal)). 
do_catch('$answer'(Goal),Goal,_,_,_):-!.
do_catch(exception(E),_,Exception,OnException,Source):-!,
  stop(Source),
  ( E=Exception->topcall(OnException)
  ; throw(E)
  ).
do_catch(Ret,_,_,_,Source):-
  % likely the result of an unusual return - we just pass it on
  stop(Source),
  return(Ret).
  
findall(X,G,Xs):-findall(X,G,Xs,[]).

findall(X,G,Xs,End):-
   new_engine(('$found'(X):-G),E),
   get(E,Answer),
   collect_all_answers(Answer,E,As,End),
   /* stop(E), % no need for this - already stopped by last answer */
   Xs=As.

% collects all answers of a Solver
collect_all_answers(no,_,Xs,Xs).
collect_all_answers(the(R),E,[X|Xs],End):-
   check_return(R,X),
   get(E,Answer),
   collect_all_answers(Answer,E,Xs,End).

check_return(R,X):-nonvar(R),R='$found'(A),!,X=A.
check_return(Ret,_):-return(Ret),fail.

gc_call(G):-findall(G,G,Gs),member(G,Gs).

/*
find_at_most(N,X,G,Xs):-find_at_most(N,X,G,Xs,[]).

find_at_most(N,X,G,Xs,End):-
  N>0,
  new_engine(('$found'(X):-G),E),
  get(E,Answer),
  collect_n_answers(Answer,N,E,As,End,State),
  if(State==stopped,true,stop(E)),
  Xs=As.
  
% collects all answers of a Solver
collect_n_answers(no,_,_,Xs,Xs,stopped).
collect_n_answers(the(R),N,E,[X|Xs],End,State):-
   N1 is N-1,
   check_return(R,X),
   get(E,Answer),
   collect_more_n_answers(Answer,N1,E,Xs,End,State).

collect_more_n_answers(Answer,0,_E,Xs,Xs,State):-if(Answer=no,State=stopped,true).
collect_more_n_answers(Answer,N1,E,Xs,End,State):-
   N1>0,  
   collect_n_answers(Answer,N1,E,Xs,End,State).

if_any(Cond,Then,Else):-
  new_engine(Cond,Cond,Engine),
  get(Engine,Answer),
  select_then_or_else(Answer,Engine,Cond,Then,Else).

select_then_or_else(no,_,_,_,Else):-Else.
select_then_or_else(the(BoundCond),Engine,Cond,Then,_):-
  backtrack_over_then(BoundCond,Engine,Cond,Then).

backtrack_over_then(Cond,_,Cond,Then):-Then.
backtrack_over_then(_,Engine,Cond,Then):-
  get(Engine,the(NewBoundCond)),
  backtrack_over_then(NewBoundCond,Engine,Cond,Then).

take_at_most(N,G):-take_at_most(N,G,G).

take_at_most(N,G,X):-
  new_engine(X,G,Engine),
  for(I,1,N),
    get(Engine,Answer),
    ( I=:=N -> !,Answer\==no,stop(Engine)
    ; true
    ),
  Answer=the(X). 
*/
    
% they use WAM-level means as change_arg/3 in has_fuel/3
% the reader is challenged to express them in classical Prolog :-)
% To do more, a separate Prolog engine or first order manipulation
% of OR continuations is needed...

% answers G as far as constraint C holds
while(C,G):-G,(C->true;!,fail).

skip_until(C,G):-G,(C->fail;!).

skip_when(C,G):-G,(C->fail;true).

% gives only the N-th answer of G
nth_answer(N,G):-N1 is N-1,Max=s(N1),skip_until(has_fuel(Max),G).

% generates at most the first N answers of G
take_at_most(N,G):-Max=s(N),while(has_fuel(Max),G).

% drops at least the first N answers of G
drop_at_least(N,G):-Max=s(N),skip_when(has_fuel(Max),G).

% re-entrant on-place counter
has_fuel(Max):-arg(1,Max,N),N>0,N1 is N-1,change_arg(1,Max,N1).

% answer_stream to list converters
find_while(C,X,G,Xs):-findall(X,while(C,G),Xs).

find_at_most(N,X,G,Xs):-findall(X,take_at_most(N,G),Xs).

all_but_at_least(N,X,G,Xs):-findall(X,drop_at_least(N,G),Xs).

% signals error if G has more than one answer
det_call(G):-find_at_most(2,G,G,Gs),!,
  ( Gs=[]->fail
  ; Gs=[G]->true
  ; % member(G,Gs),
    errmes('expected to be deterministic',G)
  ).

% SWI, SICStus, Mercury compatible if/3 construct
% which backtracks over Cond (SWI uses *-> for this)

% backtracks over Cond and calls Then each time. If Cond never succeeds, it calls Else.
if_any(Cond,Then,Else):-
  Ctr=s(0),
  ( Cond,change_arg(1,Ctr,1),Then
  ; arg(1,Ctr,0),Else
  ).

% if Cond has some answers, after exploring all with no bindings call Then, otherwise call Else
if_some(Cond,_Then,Else):-if_any(Cond,fail,true),!,Else.
if_some(_Cond,Then,_Else):-Then.

% backtracks over Goal and remembers if it had succeeded at least once. No bindings to vars in Goal are kept.
it_had_answers(Goal):- if_some(Goal,true,fail).

count_answers(Goal,Count):-
   Ctr=s(0),
   count_goal(Goal,Ctr,Count).
   
count_goal(Goal,Ctr,_):-   
    Goal,
    arg(1,Ctr,I),
    J is I+1,
    change_arg(1,Ctr,J),
  fail.
count_goal(_,Ctr,Count):-
  arg(1,Ctr,Count).

/*
it_had_answers(Goal):-
  Ctr=s(0),
  ( Goal,change_arg(1,Ctr,1),fail
  ; true
  ),
  arg(1,Ctr,1).
*/  
 
 
% generic tools

% combines 2 by 2 answers I of Generator, by applying Closure F,
% and accumulating in Final the overall result 
% no 0 element is needed as in Haskell because we initialize
% with the first solution
% if the generator is `empty' i.e if it always fails
% then fold will simply fail - this make its behvior compositional 

foldall(F,X^G,R):-
  new_engine('$found'(X),G,E),
  get(E,the(A)),
  check_return(A,R1),
  combine_with(E,F,R1,R2),
  !,
  R=R2.

combine_with(E,F,R1,R3):-
  get(E,the(A)),
  check_return(A,X),
  call(F,R1,X,R),
  !,
  R2=R,
  combine_with(E,F,R2,R3).
combine_with(_,_,R,R).

xreverse(Xs,Ys):-foldall(rcons,X^(X=[];member(X,Xs)),Ys).  

/*

% alternate implementation without engines - uses side effects

foldall(Closure,I^Generator,Final):-
  % we construct the Selector once and reuse it over and over
  term_append(Closure,args(SoFar,I,O),Selector),
  folda0(SoFar,I,O,Generator,Selector,Final).

folda0(SoFar,I,O,Generator,Selector,_):-
  inc_level(fold,Level),
  Generator,
  select_or_init(Selector,Level,SoFar,I,O),
  fail.
folda0(_,_,_,_,_,Final):-
  dec_level(fold,Level),
  bb_val(fold,Level,Final),
  rm(fold,Level).

select_or_init(Selector,Level,SoFar,_,O):-
  val(fold,Level,SoFar),!,
  Selector,
  bb_set(fold,Level,O).
select_or_init(_,Level,_,I,_):-
  bb_def(fold,Level,I).

% ensure correct implementation of embedded calls to fold/4

inc_level(Obj,X1):-val(Obj,Obj,X),!,X1 is X+1,set(Obj,Obj,X1).
inc_level(Obj,1):-def(Obj,Obj,1).

dec_level(Obj,X):-val(Obj,Obj,X),X>0,X1 is X-1,set(Obj,Obj,X1).
*/

% test

freverse(Xs,Ys):-foldall(rcons,X^(X=[];member(X,Xs)),Ys).  

rcons(Y,X,[X|Y]).



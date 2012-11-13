% THREADING API
    
current_thread(T):-
  new_java_class('java.lang.Thread',C),
  invoke_java_method(C,currentThread,T).

thread_is_alive(Thread):-
  invoke_java_method(Thread,isAlive,YesNo),
  new_java_object('java.lang.Boolean'(true),YesNo).

thread_join(Thread,MSecs):-invoke_java_method(Thread,join(MSecs),_).

thread_join(Thread):-invoke_java_method(Thread,join,_).

sleep_ms(Ms):-call_java_class_method('prolog.kernel.Machine',sleep_ms(Ms),_).

sleep(Sec):-call_java_class_method('prolog.kernel.Machine',sleep(Sec),_).

% basic thread coordination API

hub(Hub):-hub_ms(0,Hub). % timout=0 => indefinitely
hub(Sec,Hub):- '*'(Sec,1000,Ms),hub_ms(Ms,Hub).

hub_stop(H):-
  invoke_java_method(H,stop,_),
  delete_java_object(H,_).

thread_suspend(Hub):-
  hub_collect(Hub,_).

thread_resume(Hub):-
  hub_put(Hub,true).

send_term(Hub,Term):-hub_put(Hub,Term).

receive_term(Hub,Term):-hub_collect(Hub,Term).

hub_ms(MSec,H):-
  new_java_object('prolog.core.Hub'(MSec),H).

hub_collect(H,T):-
  invoke_java_method(H,collect,T).
  
hub_put(H,T):-
  invoke_java_method(H,putElement(T),_).

reset_critical(Msec):-
  call_java_class_method('prolog.core.Hub',reset_critical(Msec),_).
  
enter_critical:-
  call_java_class_method('prolog.core.Hub',enter_critical,R),
  R>0.
  
exit_critical:-
  call_java_class_method('prolog.core.Hub',exit_critical,R),
  R>0.

test_critical:-
  reset_critical(3),
  println(one),
  enter_critical,
  println(two),
  enter_critical,
  println(three),
  fail
;
  reset_critical(3),
  bg(and(enter_critical,println(bg_entered),sleep(5),exit_critical)),
  sleep(2),
  enter_critical,
  println(fg_entered),
  exit_critical,
  fail
; 
  reset_critical(3),
  enter_critical,
  sleep(4),
  exit_critical,
  println(timed_out),  
  fail
;
  println(should_be_ok),
  reset_critical(3),
  enter_critical,
  println(in_critcal),
  exit_critical.
   
    
% application - timed call

timed_call(Answer,Goal,Secs,Result):-
  Ms is Secs*1000,
  timed_call_ms(Answer,Goal,Ms,Result).

timed_call_ms(Answer,Goal,Ms,Result):-
  hub_ms(Ms,Hub),
  new_engine(ignore,call_with_hub(Hub,Answer,Goal),Engine),
  run_bg(Engine,Thread),
  % println(waiting_on=Hub+Thread),
  hub_collect(Hub,R0),
  % println(got_on_hub=R0),
  hub_stop(Hub),
  (R0=='$null'->stop(Engine),delete_java_object(Thread),Result=no;Result=R0).

call_with_hub(Hub,Answer,Goal):-
  (topcall(Goal)->Result=the(Answer); Result=no),
  % println(result=Result),
  hub_put(Hub,Result),
  fail.

/* 
  Parallel execution of a list of bg goals.
  
  All bindings are collected with prolog semantics
  i.e the goal would fail iff the corresponding
  Prolog conjunction would, except that side effects
  might occur in bg goal as goal after a failing
  goal are speculatively executed.
  
  Note that each Goal is executed as if a once(Goal)
  would be called - i.e. only the first answer is computed.
  Use findall and then explore the list of answers if
  nondet execution is desired.
  
  Not that major errors in the code will make the
  AndHub used to synchronize th bg tasks hung,
  as it waits for ALL tasks to complete.
*/

mbg(Gs):-
  mbg(Gs,Rs),
  mbg_bind(Gs,Rs).
  
mbg(Gs,Rs):-
  length(Gs,N),
  new_and_hub(N,NHub),
  mbg_all(NHub,Gs),
  and_hub_all(NHub,Rs).
  
mbg_all(NHub,Gs):-nth_member(G,Gs,I),I1 is I-1,bg(mbg_run(I1,NHub,G)),fail.
mbg_all(_,_).
  
mbg_run(I,NHub,G):-mbg_call(G,R),!,and_hub_set(NHub,I,the(R)).
mbg_run(I,NHub,_):-and_hub_set(NHub,I,no).

mbg_bind(Gs,Rs):-mbg_check(Gs,Rs),!.
mbg_bind(Gs,Rs):-errmes(mbg_failure_in(Gs),failed(Rs)).

mbg_check([],[]).
mbg_check([G|Gs],[the(G)|Rs]):-mbg_check(Gs,Rs).

mbg_call(Goal,R):-catch(and(Goal,R=Goal),Ex,and(println(error_in_mbg(Ex)),R=Ex)).

to_runnable(Goal,Runnable):-
  current_engine_object(O),
  invoke_java_method(O,to_runnable(Goal),Runnable).

% AND synchronizer

new_and_hub(Max,E):-
  new_java_object('prolog.core.AndHub'(Max),E).

and_hub_set(E,I,A):-
  invoke_java_method(E,set(I,A),_).

and_hub_add(E,A):-
  invoke_java_method(E,add(A),_).
    
and_hub_get(E,I,A):-
  invoke_java_method(E,get(I),A).

and_hub_remove(E,A):-
  invoke_java_method(E,remove,A).
  
and_hub_all(E,Os):-
  invoke_java_method(E,collectAll,T),
  T=..[_|Os].
  
and_hub_stop(E):-
  invoke_java_method(E,stop,_),
  delete_java_object(E).

new_barrier(Max,OnArrival,B):-
  runnable_engine(OnArrival,Runnable),
  new_java_object('prolog.core.Barrier'(Max,Runnable),B).

% Barrier with Runnable Action
new_barrier(Max,B):-new_barrier(Max,true,B).

barrier_arrive(B):-invoke_java_method(B,await,_).

barrier_stop(E):-
  invoke_java_method(E,stop,_),
  delete_java_object(E).
  
/* equivalent to:
new_barrier(Max,OnArrival,B):-
  runnable_engine(OnArrival,Runnable),
  new_java_object('java.util.concurrent.CyclicBarrier'(Max,Runnable),B).

new_barrier(Max,B):-new_barrier(Max,true,B).

barrier_arrive(B):-invoke_java_method(B,await,_).

barrier_reset(B):-invoke_java_method(B,reset,_).
*/

/*
  Calls Goal, if it is not in process of being executed 
  by another thread under (the same) Name. It calls Otherwise 
  if it missises the chance to run Goal.
*/

do_unless_done(Name,Goal,_Otherwise):-
  % println(trying_task(Name,Goal)),
  start_unique_task(Name,Available),
  % println(available=Available),
  Available==1,
  !,
  (Goal->Ok=yes;Ok=no),
  end_unique_task(Name),
  Ok=yes.
do_unless_done(_Name,_Goal,Otherwise):-
  Otherwise. 
   
start_unique_task(Name,Available):-
  call_java_class_method('prolog.kernel.SimpleLock',enter(Name),Available).
 
end_unique_task(Name):-
  call_java_class_method('prolog.kernel.SimpleLock',exit(Name),_).
    

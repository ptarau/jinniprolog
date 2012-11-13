/*
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
  (R0='$null'->stop(Engine),delete_java_object(Thread),Result=no;Result=R0).

call_with_hub(Hub,Answer,Goal):-
  (topcall(Goal)->Result=the(Answer); Result=no),
  % println(result=Result),
  hub_put(Hub,Result),
  fail.
*/

go:-
  % setErrmes(false),
  timed_call(X,(println(start),sleep(10),println(end),X=1),5,R),
  println(go=R).

go1:-
  stat,for(I,1,3),timed_call(ignore,(repeat,1=2),3,R),println(I=R),fail;stat.
bm:-
  println(starting(bm)),
  run_bm(T),
  println(finished(bm,total_time=T)).
  
run_bm(T):-
 ctime(T1),
 bm1,
 bm2,
 bm3,
 % bm4,
 ctime(T2),T is T2-T1.

bm1:-
 bm(speed,[nrev,boyer,allperms,bfmeta,choice,fknight,tak]).

bm2:-
 All=[differen,q8,cnrev,fibo,ffibo,
      fnrev,cal,color,lat_plan,maxlist,primes,qsort,war,
      chat,cube,fq8,lat_wam,money,puzzle,subset,assertbm,ja_primes],
 bm(more,All).

bm3:-bm(gc_and_speed,[gc]).

bm4:-bm(pereira_and_long,[lknight,backprop,pbench]).
  
bm(Topic,Names):-
  println(testing(Topic)+Names),
  member(Name,Names),
  nl,nl,
  println(running(Name)),
  ( Name:(once(go),fail;true),fail
  ; println(finished(Name))
  ),
  fail.
bm(Topic,_):-
  nl,nl,
  println(end(Topic)).

nobug:-
  bm(bug,[choice,gc]).

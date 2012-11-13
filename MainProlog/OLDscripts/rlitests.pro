xgo(N):-
  findall(I,for(I,1,N),Is),
  xwrap(Is,O),
  println(sending=N),
  ctime(T1),
  (rli_call(renoir,(println(received=N),xunwrap(O,Js),println(Js),Js=[]))->true;true),
  ctime(T2),
  T is T2-T1,
  S is N/T,
  println([time=T,performance=S]).

go(N):-
  findall(I,for(I,1,N),Is),
  println(sending=N),
  ctime(T1),
  (rli_call(renoir,Is=[])->true;true),
  ctime(T2),
  T is T2-T1,
  S is N/T,
  println([time=T,performance=S]).
  
lgo(N):-
  findall(I,for(I,1,N),Is),
  println(sending=N),
  ctime(T1),
  term_to_cat(Is,_C),
  ctime(T2),
  T is T2-T1,
  S is N/T,
  println([time=T,performance=S]).

dgo(N):-
  db_clean(xdb),
  println(sending=N),
  ctime(T1),
  foreach(for(I,1,N),db_assert(xdb,a(I))),
  db_xsave(xdb,'xdb.db'),
  ctime(T2),
  T is T2-T1,
  S is N/T,
  println([time=T,performance=S]).
  


bgo(N):-
  db_clean(xdb),
  findall(I,for(I,1,N),Is),
  println(sending=N),
  ctime(T1),
  db_assert(xdb,a(Is)),
  db_xsave(xdb,'xdb.db'),
  ctime(T2),
  T is T2-T1,
  S is N/T,
  println([time=T,performance=S]).
  
      
/*
  ?- co.
begin_compiling(to(mem),[rlitests])
end_compiling(time(31,msec),[rlitests])
yes

?- go(1000).
sending = 1000
[time = 547,performance = 1.8281535648994516]
yes

?- go(2000).
sending = 2000
[time = 2016,performance = 0.9920634920634921]
yes

?- go(3000).
sending = 3000
[time = 4938,performance = 0.6075334143377886]
yes

?- go(4000).
sending = 4000
[time = 9547,performance = 0.4189797842254111]
yes

?- go(5000).
sending = 5000
[time = 18297,performance = 0.2732688418866481]
yes

?- go(6000).
sending = 6000
[time = 27281,performance = 0.21993328690297276]
yes

?- go(8000).
sending = 8000
[time = 63968,performance = 0.12506253126563283]
yes
*/

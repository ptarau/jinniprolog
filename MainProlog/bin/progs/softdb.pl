go:-go(100000).

go1:-go(10000,10).

go(N):-
  ctime(T1),
  loop(N),
  ctime(T2),
  T is T2-T1,
  println(time(T)).

go(K,N):-
  ctime(T1),
  mloop(K,N),
  ctime(T2),
  T is T2-T1,
  println(time(T)).
  
loop(N):-
  s_assert((a(X):-b(X),c(X)),Ref),
  loop(N,Ref).
  
loop(N,Ref):-
  for(I,1,N),
   s_match(Ref,a(I),_B),
   % println(_B),
  fail.
loop(_,Ref):-
  stop(Ref).

mloop(K,N):-
  for(_,1,K),
  loop(N),
  fail.
mloop(K,N):-
  NK is N*K,
  println([asserts(K),calls(NK)]).
  
test:-test(50000).
  
test(N):-
  K=global,
  db_new_server(K),
  println('created'),
  db_query(K,assert(a(X,X)),R1),
  println(r1=R1),
  db_query(K,assert(a(10)),R2),
  println(r2=R2),
  db_query(K,pop,R3),
  println(r3=R3),
  db_query(K,clauses,R4),
  println(r4=R4),
  ctime(T1),
  tloop(N,K),
  ctime(T2),
  T is T2-T1,
  println(time(T)),
  db_server_stop(K).

tloop(N,K):-  
  for(I,1,N),
  db_query(K,assert(b(I)),_),
  fail.
tloop(N,K):-
  for(_,1,N),
  db_query(K,pop,_C),
  % println(_C),
  fail.
tloop(_,K):-
  db_query(K,clauses,Cs),
  length(Cs,L),
  println(L+Cs),
  fail.
tloop(N,K):-
  println(end_tloop(N,K)).
         
% code
 
s_assert(C,Ref):-
  new_engine(this,C,repeat,Ref).

s_match(Ref,H,B):-
  get(Ref,the((H:-B))).

% other

  
db_new_server(K):-
  this(O),
  new_engine(O,true,run_db_server(O,K),Ref),
  let(K,'$ref',Ref),
  let(K,'$obj',O),
  let(O,K,nop),
  get(Ref,_).
  
db_query(K,Cmd,R):-
  val(K,'$obj',O),
  val(K,'$ref',E),
  % statistics(htable,H1),
  let(O,K,Cmd),
  get(E,_),
  val(O,K,V),
  rm(O,K),
  % statistics(htable,H2),
  R=V,
  %stat,
  %println(H1+H2),
  true.

db_server_stop(K):-
  val(K,'$ref',Ref),
  stop(Ref),
  rm(K,'$ref'),
  rm(K,'$obj').
  
run_db_server(O,K):-
  db_server(O,K,Xs-Xs).
  
db_server(_,_,_).
db_server(O,K,S1):-
  val(O,K,Cmd),
  do_db_cmd(Cmd,R,S1,S2),
  set(O,K,R),
  db_server(O,K,S2).

do_db_cmd(assert(C),[],Xs-[C|Ys],Xs-Ys).
do_db_cmd(pop,R,CXs-Ys,Xs-Ys):-nonvar(CXs)->CXs=[C|Xs],R=the(C);CXs=Xs,R=no.
do_db_cmd(clauses,Cs,Xs-Ys,Xs-Ys):-copy_term(Xs-Ys,Cs-[]).
  
  
nobug:-
  stat,nl,
  this(O),
  N=10000,
  new_engine(O,I,for(I,1,N),E),
  for(_,1,N),
  get(E,_),
  fail.
nobug:-
  stat.
  
bug:-
  stat,
  bug(10),
  stat.
  
bug(N):-
  def(a,b,s(0)),
  for(I,1,N),
  statistics(htable,[H1,_]),
    let(a,b,s(I)),
    rm(a,b),
  statistics(htable,[H2,_]),
  H is H2-H1,
  println(htable_inc=H),
  fail.
bug(_).

bug0:-
  stat,
  bug0(10),
  stat.
  
bug0(N):-
  def(aa,b,0),
  for(I,1,N),
  statistics(htable,[H1,_]),
    set(aa,b,I),
    % rm(aa,b),
  statistics(htable,[H2,_]),
  H is H2-H1,
  println(htable_inc=H),
  fail.
bug0(_).

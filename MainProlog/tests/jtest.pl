




leak:-for(_I,1,10000),clause(append(_,_,_),_),fail.
leak:-println('use task manager').

go1:-go(0).

go1(X):-copy_term(f(X,X),C),go(C).

go(N):-
  new_engine((I:-for(I,1,N)),E),
  for(_,1,N),
  get(E,X),
  println(X),
  fail.
go(N):-
  println(end_answers_collected(N)).

go:-ctime(T1),go(100000),ctime(T2),T is T2-T1,println(time(T)).

bug:-
  N is 3,
  new_engine( (I:-for(I,1,N) ),E),
  for(_,1,N),
  ask_engine(E,X),
  println(X),
  fail.


undef:-
 a,
 b.
 
a.
 
first:- 
%bad(X),
 println(X).

nogood(99).


file2str(F,S):-
  file2chrs(F,Cs),
  atom_codes(S,Cs).
  
file2chrs(FName,Cs):-   seeing(Other),
   see(FName),
   get0(C),fcollect(C,Cs),
   seen,   see(Other).

fcollect(-1,[]):- !.
fcollect(X,[X|Xs]):-
   get0(NewX),
   fcollect(NewX,Xs).
fread_all:-
  kernel_files(Kernel,Extra),  extra_files(Extra),
  member(F,Kernel),  sread(F,_),
  println(F),  fail.
fread_all.
fread_all:-
  kernel_files(Kernel,Extra),  extra_files(Extra),
  member(F,Kernel),  sread(F,C),
  println(C),  fail.
fread_all.

fread:-
  ctime(T1),
  fread_all,
  ctime(T2),
  T is T2-T1,
  println(time(T)).
  tread(F):-
  tell('bad.txt'),  fopen(F,r,H),
  repeat,
    fread(H,C),
    (C=end_of_file->true;pp_clause(C)),
  !,
  fclose(H),
  told,
  tell('good.txt'),
  see(F),
  repeat,
    read(X),
    (X=end_of_file->true;pp_clause(X)),
  !,
  seen.

qtest:-  X='AB''C',
  println(X).
  rtest:-
  new_engine(Y,mreturn(Y),E),
  element_of(E,A),
  println(A),
  fail.rtest:-println(end). mreturn(Y):-   member(X,[a,b,c]),
   return(just_starting(X)),     member(Y,[1,2]).
   
catchtest:-
  catch(ctry(Y),abort(X),println(aborted(X))),  println(end_of_branch(Y)),
  fail;
  println(end).
cmes:-  new_engine(errmes(here,there),EO),  element_of(EO,X),  println(X).ctry(Y):-
  member(Y,[1,2,3,4,5]),  println(trying(Y)),
  (Y>2->throw(gabort(Y))
  ; true
  ). 
dtest:-  answer_source(X,and(new_db(X),true),R),  get(R,the(Db)),  call_source(db_add(Db,a(1))),  call_source(db_collect(Db,a(_),Bs)),
  println(Bs).  

% compares clause readersecompare:-ecompare(A,A).
ecompare(A,B):-  G=member(X,[1,2,3]),H=for(Y,1,3),new_engine(X,G,E1),new_engine(Y,H,E2),same_in(E1,E2,A,B).nclause(C):-
  kernel_files(Kernel,['extra.pl']),
  member(F,Kernel),
  new_string_clause_reader(F,R),  element_of(R,S),
  sread(S,C-_).  
jdif(R):-  new_engine(O,oclause(O),EO),
  new_engine(N,nclause(N),EN),  same_in(EO,EN,CO,CN),
  if(eq(CO,CN),
    eq(R,same(CO)),
    eq(R,dif(CO,CN))
  ). 
fdif(F,R):-  new_engine(O,oclause_of(F,O),EO),
  new_engine(N,clause_of(F,N),EN),  same_in(EO,EN,CO,CN),
  if(eq(CO,CN),
    eq(R,same(CO)),
    eq(R,dif(CO,CN))
  ).  
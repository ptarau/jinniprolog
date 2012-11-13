/*
  todo: :-initialization(Goal). to be handled like in ISO Prolog
  after the (complete) inhertance tree is loaded
  
includes_preds_clause_stream_of
*/

itest:-scan_super(vtetris,Xs),
    forall(member(X,Xs),println(X)).
     
t1:-
  qsort:qsort([b,a,c,d,a],S),
  println(S),
  nrev:nrev(S,R),
  println(R),
  qsort:assert(a(1)),
  nrev:assert(a(2)),
  qsort:assert(a(3)),
  nrev:a(X),
  forall(
    qsort:a(Y),
    println(first=Y)
  ),
  println(second=X).


% seems to happen as a result of a diff between mjc.bat and mjinni.bat

ok1:-find_file('http://daisy:81/jinnidemo/progs/hello.pl',F),println(F).


bug1:-see('../progs/hello.pl'),repeat,read(X),println(X),X=end_of_file,!,seen.

bug2:-see('http://daisy:81/jinnidemo/progs/hello.pl'),repeat,read(X),println(X),X=end_of_file,!,seen.

ok2:-term_of('http://daisy:81/jinnidemo/progs/hello.pl',T),println(T),fail.

t2:-
  new_char_reader('http://www.binnetcorp.com/download/jinnidemo/progs/nrev.pl',Reader),
  forall(element_of(Reader,C),put(C)).

t3:-
  'http://www.binnetcorp.com/download/jinnidemo/progs/nrev.pl':go.

t4:-
  'http://www.binnetcorp.com/download/jinnidemo/progs/nrev':go.

t5:-
  rollback,
  qcompile('../progs/nrev.pl'),go.

t6:-
  ['http://www.binnetcorp.com/download/jinnidemo/progs/nrev.pl'],go.

t7:-
  term_of('http://www.binnetcorp.com/download/jinnidemo/progs/nrev.pl',T),
  println(T),
  !.




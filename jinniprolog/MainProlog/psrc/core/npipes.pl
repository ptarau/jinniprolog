% Linda-like named pipe operations: note that only the top functor is used
% that assumes for rd/out operations that all args are free variables

% SIMPLE LINDA-LIKE API FOR NAMED PIPES

% waits to get a term, when available
in_term(Term):-functor(Term,F,N),in_term(N,F,Term).

% makes a term available but waits if a previous result is not consumed
out_term(Term):-functor(Term,F,N),out_term(N,F,Term).

% waits to get a term and puts it back
rd_term(Term):-functor(Term,F,N),rd_term(N,F,Term).

clear_terms(Term):-functor(Term,F,N),clear_terms(N,F).

% POWER USER LINDA-like API

% weak indexed Linda operations: O(1) for given <Key1,Key2>
% unification is not used for search - make sure Key1 and Key2 are known

% waits to get a term, when available
in_term(Key1,Key2,Term):-global_hub(Key1,Key2,H),receive_term(H,T),should_unify(T,Term).

% makes a term available
out_term(Key1,Key2,Term):-global_hub(Key1,Key2,H),send_term(H,Term).

% waits to get a term and puts it back
rd_term(Key1,Key2,Term):-in_term(Key1,Key2,T),out_term(Key1,Key2,T),should_unify(T,Term).

% frees thread waiting on the Hub and discards the Hub
clear_terms(K1,K2):-K=hub(K1,K2),get_global_prop(K,H),H\=='$null',rm_global_prop(K),hub_stop(H).

% creates a Hub unless it is already associated to Key1, Key2
global_hub(K1,K2,H):-K=hub(K1,K2),get_global_prop(K,X),X\=='$null',!,X=H.
global_hub(K1,K2,H):-K=hub(K1,K2),hub(H),set_global_prop(K,H).

should_unify(T,T):-!.
should_unify(T,Term):-errmes(receive_error,should_unify(received=T,expected=Term)).

% tests
npipes_test1:-
  bg((in_term(a(X)),println(X))),sleep(3),out_term(a(6)).

% speed is in the 50000/sec
npipes_test:-npipes_test(10).

% speed around 3000-5000/sec total in+out on a slow PC
npipes_test(N):-
  ctime(T1),
  bg((
      for(I,1,N),
      %println(before(in(I))),
      in_term(a(X)),
      println(X),
      fail
    ),
    Thread
  ),
  ( for(I,1,N),
    out_term(a(I)),
    % println(after(out(I))),
    fail
  ; true
  ),
  ctime(T2),
  T is T2-T1,
  println(joining_thread),
  thread_join(Thread),
  sleep(1),
  println(total_time_for(N,ms=T)).



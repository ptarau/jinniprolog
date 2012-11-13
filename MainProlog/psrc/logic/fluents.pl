% fluent operations (mostly on Sources)

element_of(I,X):-pick_element_of(I,X).
% element_of(I,_):-stop(I),fail.
pick_element_of(I,X):-get(I,the(A)),select_from(I,A,X).

select_from(_,A,A).
select_from(I,_,X):-pick_element_of(I,X).

good_element_of(E,X):-element_of(E,X),check_element(E,X).

check_element(E,X):-nonvar(X),X=exception(Exception),stop(E),!,throw(Exception).
check_element(_,_).

split_source(S,S1,S2):-
  findall(NewS,nth_source_clone(2,S,NewS),[S1,S2]).

split_source(S,S1,S2,S3):-
  findall(NewS,nth_source_clone(3,S,NewS),[S1,S2,S3]).

/* could be done lazily - with a table for unique elements 
  i.e. by implementing an ObjectDict for canonicalization in Java 
*/
  
uniquify_source(S,SWithNoDups):-
  findall(E,element_of(S,E),Es),
  sort(Es,Sorted),
  new_engine(X,member(X,Sorted),SWithNoDups).
  
nth_source_clone(N,S,NewS):-
  findall(X,element_of(S,X),Xs),
  for(_,1,N),
  new_engine(X,member(X,Xs),NewS).
  
compose_sources(S1,S2,S):-new_engine(X,element_of_either(S1,S2,X),S).

element_of_either(S1,_,X):-element_of(S1,X).
element_of_either(_,S2,X):-element_of(S2,X).

% fold operations on engines
efoldl(Engine,F,R1,R2):-get(Engine,X),efoldl_cont(X,Engine,F,R1,R2).

efoldl_cont(no,_Engine,_F,R,R).
efoldl_cont(the(X),Engine,F,R1,R2):-
  check_element(Engine,X),
  call(F,R1,X,R),
  efoldl(Engine,F,R,R2).

ereverse(Xs,Ys):-
  new_engine(X,member(X,Xs),E),
  efoldl(E,rcons,[],Ys).  
  
efltest(R):-new_engine(I,for(I,1,5),E),efoldl(E,-,0,R).
%R = -15

efoldr(Engine,F,R1,R2):-
  get(Engine,X),
  efoldr_cont(X,Engine,F,R1,R2).

efoldr_cont(no,_Engine,_F,R,R).
efoldr_cont(the(X),Engine,F,R1,R2):-
  check_element(Engine,X),
  efoldr(Engine,F,R1,R),
  call(F,X,R,R2).

efrtest(R):-new_engine(I,for(I,1,5),E),efoldr(E,-,0,R).
%R = 3

% seems unused

same_in(I,J,X,Y):-
  get(I,the(A)),
  get(J,the(B)),
  select_in(I,J,A,B,X,Y).

select_in(_,_,A,B,A,B).
select_in(I,J,_,_,X,Y):-same_in(I,J,X,Y).

% streams

new_stream(X,G,'$s'(A,E)):-new_engine(X,G,E),get(E,the(A)).

stream_now('$s'(A,_),A).

stream_change(S):-arg(2,S,E),get(E,the(A)),change_arg(1,S,A). 

nat_stream(N,S):-Max is N-1,new_stream(I,for(I,0,Max),S).

nat_test:-
  N=5,
  nat_stream(N,S),
  stream_now(S,H1),
  stream_now(S,H2),
  stream_change(S),
  stream_now(S,H3),
  println(S+[H1=H2,H3]).
  
  
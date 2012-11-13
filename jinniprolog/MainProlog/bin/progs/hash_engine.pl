/*
  Abstract queue API using engines as server agents
*/

% creates a new engine server providing hash map operations
engine_server_new_map(Tb):-new_engine(done,engine_server_hash_map(255),Tb).
engine_server_new_map(N,Tb):-new_engine(done,engine_server_hash_map(N),Tb).

% adds a key-value pair to the end
engine_add_element(Tb,K,X):-engine_server_call(Tb,add_element(K,X),the(yes)).

% removes a key-value pair from the front
engine_remove_element(Tb,K,X):-engine_server_call(Tb,remove_element(K,X),the(yes(K-X))).

% adds a key-value pair to the front
engine_push_element(Tb,K,X):-engine_server_call(Tb,push_element(K,X),the(yes)).

% returns a copy of the list of values associated to a key
engine_map_values(Tb,K,Xs):-engine_server_call(Tb,queue(K),the(Xs)).

% delete a key-value pair
engine_delete_element(Tb,K,X):-engine_server_call(Tb,delete_element(K,X),the(yes(K-X))).

% prints out statistics of the engine server
engine_statistics(Tb):-engine_server_call(Tb,invoke_server_task(_,statistics),_).

% stops and reclains resources of the engine server
engine_server_stop(Tb):-stop(Tb).

/*
  Helper predicates/Implementation
*/

engine_server_hash_map(N):-
  functor(D,'$',N),
  engine_server_hash_map(D,N).
    
engine_server_hash_map(D,N):-
  from_engine(Q),
  arg(1,Q,K),
  term_hash(K,I0),I is 1+(I0 mod N),
  arg(I,D,S1),
  (var(S1)->S1=Xs-Xs;true),
  engine_server_task(Q,S1,S2,A),
  change_arg(I,D,S2),
  % println(here=>D),
  return(A),
  engine_server_hash_map(D,N).

engine_server_task(push_element(K,X),S1,S2,yes):-push_element(K-X,S1,S2).
engine_server_task(add_element(K,X),S1,S2,yes):-add_element(K-X,S1,S2).
engine_server_task(remove_element(K,X),S1,S2,YesNo):-engine_server_task_remove(K-X,S1,S2,YesNo).
engine_server_task(queue(K),S,S,KsXs):-findall(K-X,(S=Xs-[],member(K-X,Xs)),KsXs).
engine_server_task(delete_element(K,X),S1,S2,YesNo):-engine_server_task_delete(K-X,S1,S2,YesNo).
engine_server_task(invoke_server_task(_K,Goal),S,S,Goal):-if(Goal,true,true).

add_element(X,Xs-[X|Ys],Xs-Ys).
push_element(X,Xs-Ys,[X|Xs]-Ys).

engine_server_task_remove(X,Xs-Ys,NewXs-Ys,YesNo):-nonvar(Xs),Xs=[X|NewXs],!,YesNo=yes(X).
engine_server_task_remove(_,XsYs,XsYs,no).

engine_server_task_delete(X,Xs-Ys,NewXs-Ys,YesNo):-nonvar_select(X,Xs,NewXs),!,YesNo=yes(X).
engine_server_task_delete(_,XsYs,XsYs,no).

engine_server_task_stop(Tb):-stop(Tb).

engine_server_call(Tb,Op,R):-to_engine(Tb,Op),get(Tb,R).



/*
  tests
*/
go:-
  engine_server_new_map(4,Tb),
  engine_add_element(Tb,k(1),a(X,X)),
  engine_add_element(Tb,k(1),b(X,c(X))),
  engine_add_element(Tb,k(2),b(99)),
  engine_push_element(Tb,k,77),
  engine_push_element(Tb,k,88),
  engine_map_values(Tb,k,Xs),
  println(first=Xs),
  engine_delete_element(Tb,k(2),b(R)),
  println(del=R),
  engine_remove_element(Tb,k(1),D),
  println(remove_element=D),
  engine_map_values(Tb,k,Ys),
  println(last=Ys),
  engine_statistics(Tb).


go1:-
  engine_server_new_map(Tb),
  engine_server_call(Tb,add_element(k,a(X,X)),AE),
  println(AE),
  engine_server_call(Tb,add_element(k,b(99)),AE1),
  println(AE1),
  engine_server_call(Tb,push_element(k,77),AE),
  println(AE),
  engine_server_call(Tb,queue(k),Xs),
  println(Xs),
  engine_server_call(Tb,delete_element(k,_),R),
  println(R),
  engine_server_call(Tb,remove_element(k),D),
  println(D),
  engine_server_call(Tb,statistics,_).

go2:-
   new_engine(X,for(X,1,4),E),
   /*
   foreach(
     element_of(E,A),
     println(A)
   ),
   */
   get(E,Z),println(Z),
   reuse_engine(X,member(X,[a,b,c]),E),
   foreach(
     element_of(E,A),
     println(A)
   ).  
  
/*
  benchmarks
*/
 
bm:-
  bm(20000).
  
bm(N):-
  time_goal(bm1(N)),
  time_goal(bm2(N)),
  time_goal(bm3(N)).

% prolog API queues
bm1(N):-
  (val(b,b,_)->rm(b,b);true),
  for(I,0,N),
  xqpush(b,b,b(I)),
  xenq(b,b,b(I)),
  xdeq(b,b,_),
  xdeq(b,b,_),
  I=N.

% engine server queues    
bm2(N):-
  engine_server_new_map(Tb),
  for(I,0,N),
  engine_push_element(Tb,k(I),b(I)),
  engine_add_element(Tb,k(I),b(I)),
  engine_remove_element(Tb,k(I),_),
  engine_remove_element(Tb,k(I),_),
  I=N,
  % engine_statistics(Tb),
  engine_server_stop(Tb).

% prolog database queues  
bm3(N):-
  (val(a,a,_),rm(a,a)->true;true),
  for(I,0,N),
  xqpush(a,a,a(I)),
  xenq(a,a,a(I)),
  xdeq(a,a,_),
  xdeq(a,a,_),
  I=N.

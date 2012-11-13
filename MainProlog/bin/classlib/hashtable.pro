/*
 Example of wrapper for a Java class as a Prolog class
 Uses some functionality inherited from the wrapper
 java_object which reflects the Java calls "Object"

*/

:-[java_object].

hashtable:-
  new_java_class('java.util.Hashtable',C),
  new_java_object(C,void,JavaHashTable),
  object<=JavaHashTable.

hashtable(Alist):-
  (Alist=[];Alist=[_|_]),
  !,
  hashtable,
  from_alist(Alist).
hashtable(JavaHashTable):-
  object<=JavaHashTable.

put_data(Key,Data):-
  object=>T,
  invoke_java_method(T,put(Key,Data),_Result).

put_data(Key-Data):-
  put_data(Key,Data).
    
get_data(Key,Data):-
  object=>T,
  invoke_java_method(T,get(Key),Data).
 
remove_data(Key):-
  object=>T,
  invoke_java_method(T,remove(Key),_Result).

to_pair(K,V):-
  to_enumeration(E),
  E:backtrack_at(K),
  get_data(K,V).

to_alist(KsVs):-findall(K-V,to_pair(K,V),KsVs).
  
from_alist(KsVs):-
  map(put_data,KsVs).

to_enumeration(E):-
  object=>V,
  invoke_java_method(V,keys,VE),
  new(enumeration(VE),E).
           
test:-
  ( 
    statistics(global_stack,[G1,_]),
    put_data(hello,f(X,X)),
    put_data(bye,g(X,X)),
    statistics(global_stack,[G2,_]),
    println(heap_before(G1-G2)),
    toString(S),
    println(S),
    to_alist(L),
    println(L),
    fail
  ; println(getting_term_on_different_branch),
    statistics(global_stack,[G3,_]),
    get_data(hello,R),
    R=f(g(_),g(_)),
    statistics(global_stack,[G4,_]),
    println(heap_after(G3-G4)),
    println(R)
  ),
  etest.

etest:-
   to_enumeration(E),
   repeat,
    ( E:hasMoreElements->
      E:nextElement(Key),
      get_data(Key,X),
      println(enum=Key-X),fail
    ; true
    ),
    !.

btest:-
   to_enumeration(E),
   E:backtrack_at(Key),
   get_data(Key,Data),
   println(Key-Data),
   fail
;  println(done).
   
htest(N):-
  ctime(T1),
  htest0(N),
  ctime(T2),
  println(time(T,puts(N))),
  ctime(L1),
  ctime(L2),
  
  for(I,1,N),
  fail.

btest(N):-
  ctime(T1),
  btest0(N),
  ctime(T2),
  println(time(T,puts(N))).
  
  for(I,1,N),
  fail.

/*

?- new(hashtable,H),H:put_data(hello,99),H:get_data(hello,X).
H = '$instance'(hashtable@554,1) X = 99 ;

?- new(hashtable([a-1,b-2]),H),H:etest.
enum = (b - 2)
enum = (a - 1)
H = '$instance'('$object'(979),1) ;
no

?- new(hashtable([a-1,c-3,b-2]),H),H:btest.
b - 2
a - 1
c - 3
done
H = '$instance'('$object'(983),1) ;
no

*/
   
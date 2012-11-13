:-[list].

slow_queue:-list.

add(Element):-extendWith([Element]).

remove(Element):-pop(Element).

/*

?- new(slow_queue,Q),Q:add(1),Q:add(2),Q:remove(X),Q:show.
[2]
Q = '$instance'(slow_queue@698,3) X = 1 ;
no
?-

*/

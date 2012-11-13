:-initialization((
   println(initializing),
   assert(initialized(cf)),
	 cf <== 99,
	 cf ==> X,
	 println(cf=X)
)).

cfield :- if<=88.

get_cf(Value) :- cf ==> Value.
get_if(Value) :- if => Value.

bug:-
  pp('start'),
  get_if(IF),
  pp(got_if=IF),
  ( get_cf(CF)->
    pp(got_cf=CF)
  ; pp(bug_this_should_not_fail)
  ).
  
/*
?- new(cfield,C),C:bug.
initializing
start.
'$field'(if).
got_if = 88.
no
*/
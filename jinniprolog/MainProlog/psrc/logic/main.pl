is_prolog(X):-bb_val(is,prolog,Other),!,X=Other.
is_prolog(jinni_compiled).

% TOPLEVEL

main(Answer):-
  run(_,Answer).
  
% real entry point: G_prolog_main run/3 search: G_main

run(Query,Answer):-is_compiled(top_run(_,_)),!,call(top_run(Query,Answer)).
run((Answer:-Query),Answer):-!,topcall(Query).
run(Query,Answer):-topcall(Query),Answer=Query.

topcall(Goal):-metacall(Goal).
    
halt:-halt(0).

% halt(Code):-throw(halt(Code)).

halt(C):-jcall(halt,C,_).


% DELEGATION OF UNDEFINED CALLS

% possibly trims continuation
% then call handler for undefined calls

/*
'$undefined'(N,FXsC):-
   class_name(ThisName),
   functor(FXsC,F,K),
   println('$undefined'(F/K,ThisName),N,FXsC)),fail.
*/
'$undefined'(0,FXsC):-
   !,
   FXsC=..[F|XsC],
   XsC=[_|_],
   append(Xs,[_],XsC),
   !,
   FXs=..[F|Xs],
   '$handle_undefined'(FXs).
'$undefined'(1,FXs):-
   '$handle_undefined'(FXs).
   
% calls super or meta-interpreter

% '$handle_undefined'(G):-is_compiled(integrity_constraint(_,_,_)),!,try_abducible(G).
'$handle_undefined'(G):-do_goal(G).


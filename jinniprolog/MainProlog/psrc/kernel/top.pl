% overrides main.pl run/3 if defined, search G_prolog_main, G_main

top_run(SQuery,Answer):-
  ( var(SQuery)->
    G=toplevel  
  ; atomic(SQuery)->
    sread_goal(SQuery,Bs,Vs),
    ( Bs=(_ :- _)->G=Bs
    ; G=(Vs:-Bs)
    )
  ; G=SQuery  
  ),
  ( G=(Vars:-Query)->Answer=Vars  
  ; Query=G,Answer=G  
  ),
  topcall(Query).
  
toplevel:-
  Query=Body-Vs,
  repeat,
	telling(O),tell(user),
	prompt('?->'),
	sread0(false,true,user,Query),
	topinterp(Body,Vs,O),
  !.

scall(SGoal,A):-
  sread_goal(SGoal,G,VsEs),
  shell_filter(G,VsEs,A).

% user hook - it can ensure security by only allowing a set of known calls
shell_filter(G,VsEs,Result):-is_compiled(shell_call(_,_,_)),!,call(shell_call(G,VsEs,Result)).
shell_filter(G,VsEs,A):-default_shell_action(G,VsEs,A).

default_shell_action(G,VsEs,A):-
  % let exceptions/returns get to Java - which should restart machine
  % NOT wrapped_call(G): catch may leak engine on success
  topcall(G),
  (
    VsEs==[]->  !, A=yes
  ; numbervars(G,1,_),
    ( A=new
    ; member(V=E,VsEs),
      ( A=(=)
      ; A=V
      ; A=E
      )
    )
  ).
default_shell_action(_,_,no).  

shutdown:-shutdown(0).

shutdown(Code):-jcall(shutdown,Code,_).

end_of_file :- halt.
		
topinterp(end_of_file,_,_):-!,end_of_file.
topinterp(Goal,Vs,O):-
	tell(O),
	report_answers(Vs,Goal,Ok),
	telling(F),tell(user),write(Ok),nl,tell(F),
	fail.

report_answers([],Goal,yes):-
	wrapped_once(Goal),!.
	% topcall(Goal),!.
report_answers([V|Vs],Goal,Answer):-
	wrapped_call(Goal),
	% topcall(Goal),
		telling(F),
		tell(user),
		report_top_vars([V|Vs]),
		user_stop(Stop),
	  ( Stop=yes->!,Answer=no
	  ; true
	  ),
	  nl,
	  tell(F),
	  Answer==no.
report_answers(_,_,no).

report_top_vars(Eqs):-
  [B]=" ",
	member(V=E,Eqs),
	prompt(V),prompt('='),writeq(E),put(B),
	fail.
report_top_vars(_).

prompt(X):-
  atom_codes(X,Cs),
  member(C,Cs),
  put(C),
  fail.
prompt(_):-
  [B]=" ",
  put(B),
  flush.

wrapped_once(Goal):-
  catch_once(topcall(Goal),E,top_exception(E,Goal)).

wrapped_call(Goal):-
  catch(topcall(Goal),E,top_exception(E,Goal)).
  
top_exception(E,_):-
  nonvar(E),
  E=halt(Code),
  !,
  throw(halt(Code))
  % ,abort
  ,fail
  .
top_exception(E,G):-
  nonvar(E),nonvar(G),
  println(uncaught_exception(E,in(G)))
  %,abort
  ,fail
  .
 
interactive(X):-var(X),!,val(flag,interactive,X).
interactive(X):-let(flag,interactive,X).

interactive:-val(flag,interactive,yes).

user_stop(yes):-
  interactive,
  !,
  telling(F),tell(user),prompt('; more answers (y/n)?'),tell(F),
  seeing(F),
    see(user),get(X),
  see(F),
  [X]="n",
  seeing(F1),
    see(user),repeat,get0(10),!,
  see(F1).
user_stop(no):-
  telling(F),
  tell(user),
  [X]=";",
  put(X),
  tell(F).

/*
icall(engine,E,OpArgs):-icall_engine(OpArgs,E).

icall_engine(open(XG),'$interactor'(engine,E)):-this_class(O),create_engine(O,XG,E).
icall_engine(reuse(XG),E):-O is 0-E,create_engine(O,XG,_R).
icall_engine(next(R),E):-get(E,R). % in
icall_engine(close,E):-stop(E).
icall_engine(tell(T),E):-to_engine(E,T). % out

engine_yield(R):-return(R). % out
engine_ask(X):-from_engine(X). % in

Interactor@Goal :- 
  nonvar(Interactor),
  Interactor='$interactor'(Type,Id),
  !,
  icall(Type,Id,Goal).
    
new_interactor(IType,Answer,Goal, Interactor):-
  IType=engine,
  Interactor=interactor(engine,E),
  this_class(O),
  create_engine(O,(Answer:-Goal),E).
  
reuse_interactor(Interactor,Answer,Goal):-
  Interactor=interactor(engine,E)
  O is 0-E,
  create_engine(O,(Answer:-Goal),_R).
  
stop_interactor(Interactor):-
  Interactor=interactor(engine,E),
  stop(E).

interactor_yield(Answer):-
  from_engine((Answer:-Goal)),
  call(Goal),
  return(Answer).

ask_interactor(Interactor,Goal,AnswerPattern):-
  Interactor=interactor(engine,E),
  to_engine(E,Goal),
  get(E,AnswerPattern).

*/

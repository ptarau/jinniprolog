
  % ------------------------------ IO ----------------------------------
% shared IO operations - work in both the interpreter and the compiler
% various forms of read, write etc

reconsult(F):-
  find_file(F,File),
  println(reconsulting(File)),
  file_clause_reader(File,S),
  reconsult_source(S).

consult(F):-
  find_file(F,File),
  println(consulting(File)),
  file_clause_reader(File,S),
  consult_source(S).
  
unconsult(F):-
  find_file(F,File),
  println(unconsulting(File)),
  file_clause_reader(File,S),
  un_consult_source(S).

consult_action(':-'(G)):-if(G=[F],consult(F),once(topcall(G))).
consult_action(':-'(H,B)):-assert(':-'(H,B)).
  
un_consult_action((':-'(G))):-if(G=[F],unconsult(F),true).
un_consult_action(':-'(H,_)):-
  % println(uncons(H)),
  functor(H,F,N),
  abolish(F,N).
  
file_clause_reader(File,Handle):-
  new_engine(C,clause_of(File,C),Handle).
  

% basic consult operations

consult_source(S):-
  foreach(
    good_element_of(S,C),
    consult_action(C)
  ).

un_consult_source(S):-
  foreach(
    good_element_of(S,C),
    un_consult_action(C)
  ).
  
reconsult_source(S):-
  split_source(S,S1,S2),
  un_consult_source(S1),
  consult_source(S2).


% consulting from files or URLs - provided as fast builtins
reconsult_string(S):-
  string_clause_reader(S,R),
  reconsult_source(R).

% db io

db_save(File):-
  this_db(Db),
  db_save(Db,File).

db_load(File):-
  this_db(Db),
  db_load(File,Db).
      
db_save(Db,File):-
  [Dot]=".",
  telling(CF),
  tell(File),
  ( db_clause(Db,H,B),
    writeq((H:-B)),put(Dot),nl,
    fail
  ; true
  ),
  told,
  tell(CF).
  
db_load(File,Db):-
  db_clean(Db),
  db_consult(File,Db).

db_consult(File,Db):-
  foreach(
    clause_of(File,C),
    db_consult_action(C,Db)
  ).

db_consult_action(':-'(G),Db):-if(G=[F],db_consult(F,Db),once(topcall(Db,G))).
db_consult_action(':-'(H,B),Db):-db_assert(Db,':-'(H,B)).

listing:-listing(_).

listing(Pattern):-this_db(Db),db_listing(Db,Pattern).

db_listing(Db):-db_listing(Db,_).

db_listing(Db,FN):-var(FN),!,db_listing0(Db,FN).  
db_listing(Db,F):-atom(F),!,db_listing0(Db,F/_).
db_listing(Db,FN):-is_functor(FN),!,db_listing1(Db,FN).
db_listing(Db,FN):-errmes(bad_argument_for_listing,Db:(FN)).

db_listing1(Db,F/N):-
  functor(H,F,N),
  C=(H:-B),db_clause(Db,H,B),
  pp_clause(C),
  fail.
db_listing1(_,_).  
  
db_listing0(Db,FN):-
  db_get_preds(Db,FNs),
  member(FN,FNs),
  db_listing(Db,FN),
  fail.
db_listing0(_,_).

make_compilable(Files,File):-
  Db='$temp',
  db_clean(Db),
  (atomic(Files)->Fs=[Files];Fs=Files),
  foreach(member(F,Fs),db_consult(F,Db)),
  db_save(Db,File),
  db_clean(Db).  
    
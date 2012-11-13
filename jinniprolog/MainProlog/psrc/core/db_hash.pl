% generalized indexing on ground term key
      
% adds one
hash_put(GroundKey,Term):-
  term_hash(GroundKey,K),
  db_assert(K,'$hashed'(GroundKey,Term)),
  add_hash_key(K).

% gets all
hash_get_all(Key,Ts):-findall(T,hash_get(Key,T),Ts).

hash_count(Key,N):-count_answers(hash_get(Key,_),N).

% gets each
hash_get(Key,Term):-var(Key),!,get_hash_key(K),hash_get0(K,Key,Term).
hash_get(GroundKey,Term):-nonvar(GroundKey),term_hash(GroundKey,K),hash_get0(K,GroundKey,Term).

hash_get0(K,GroundKey,Term):-
  db_clause(K,'$hashed'(GroundKey,T),true),
  Term=T.

% removes first  
hash_rm(GroundKey,Term):-
  term_hash(GroundKey,K),
  db_retract1(K,'$hashed'(GroundKey,Term)),
  del_hash_key(K).

% removes all for a key
% hash_clear(Key):-var(Key),!,hash_clear.
hash_clear(GroundKey):-
  term_hash(GroundKey,K),
  db_retractall(K,'$hashed'(GroundKey,_)),
  del_hash_key(K).

% removes all from all keys
hash_clear:-get_hash_key(K),db_clean(K),del_hash_key(K),fail.
hash_clear.

hash_save(File,P):-
  tell(File),
  functor(Rel,P,2),
  arg(1,Rel,K),
  arg(2,Rel,Cs),
  findall(K,get_hash_key(K),Us),
  sort(Us,Ks),
  member(K,Ks),
    findall(Key-Term,hash_get0(K,Key,Term),KsTs),
    findall(Key-Term,
      keygroup(KsTs,Key,Term),
    Cs),
    pp_clause(Rel),
  fail.
hash_save(_,_):-
  told.
    
get_hash_key(K):-db_clause('$hashkeys','$key'(K),true).

add_hash_key(K):-val('$hashkey',K,yes),!.
add_hash_key(K):-def('$hashkey',K,yes),db_assert('$hashkeys','$key'(K)).

del_hash_key(K):- \+(val('$hashkey',K,yes)),!.
del_hash_key(K):- \+(db_clause(K,'$hashed'(_,_),true)),!,rm('$hashkey',K),db_retractall('$hashkeys','$key'(K)).

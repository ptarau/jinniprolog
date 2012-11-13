:-op(850,fx,'?').

% implementation

% mark as abducible
?G --> [G].

% runs Goal and collects the abducibles As 
% that have satisfied all the constraints

abduce(Goal,As):-
  phrase(Goal,As,[]),  
  \+check_impossible(As),
  \+ \+ check_necessary(As),
  \+ \+ check_majority(As).

% runs goal and prints out abducted facts  
abduce(Goal):-
  foreach(
    abduce(Goal,As),
    pp_clause(As=>Goal)
  ).

% checks an imposibility constraint - this must fail
% it fails if any set of imposibile facts get abduced
% (note: failure is translated into succees by callee) 
check_impossible(As):-
  call_ifdef(impossible(Is,[]),Present=no),
  (Present=yes->included(Is,As);fail).

% checks a necesity constraint - this must succeed  
% succeeds if all necessary facts are among the abduced
check_necessary(As):-
  call_ifdef(necessary(Ns,[]),Present=no),
  (Present=yes->included(Ns,As);true). 

% checks a majority constraint - this must succeed 
% succeds if such constraints are absent or if
% a majority above threshold of abducibles is found 
check_majority(As):-
  call_ifdef(majority(Threshold,Ns,[]),Present=no),
  (Present=yes->check_majority(Threshold,As,Ns);true).
  
check_majority(Threshold,As,Ns):-  
  intersection(Ns,As,Cs),
  length(Cs,LC),
  length(Ns,LN),
  R is LC/LN,
  R>=Threshold.

% tools: assume ground facts

% checks set inclusion
included(Is,As):-for_all(member(I,Is),member(I,As)).

% computes set intersection
intersection([],_Bs,[]):-!.
intersection([A|As],Bs,Cs):- \+(member(A,Bs)),!,intersection(As,Bs,Cs).
intersection([A|As],Bs,[A|Cs]):-intersection(As,Bs,Cs).
  
% end
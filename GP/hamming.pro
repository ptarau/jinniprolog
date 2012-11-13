% hamming distance based "closeness" relations

hamming_split(A,On,Off):-
  hamming_split(A,[On],[Off],OnDist,OffDist,R),
  write('in: '),lpp(A),
  write('on: '=OnDist),lpp(On),
  write('off:'=OffDist),lpp(Off),
  println(split=R).

hamming_split(A,OnSet,OffSet,OnDist,OffDist,R):-
  hamming_min(A,OnSet,OnDist),
  hamming_min(A,OffSet,OffDist),
  R is OffDist/(OnDist+OffDist).

% hamming distance variants from point to set

hamming_min(B,[A|As],S):-hamming(B,A,D),hamming_min(As,B,D,S).

hamming_min([],_,S,S).
hamming_min([A|As],B,S1,S2):-
  hamming(A,B,D),
  min(S1,D,S),
  hamming_min(As,B,S,S2).

hamming_avg(A,Bs,R):-
  hamming_sum(A,Bs,S),
  length(Bs,L),R is S/L.

hamming_sum(A,Bs,S):-hamming_sum(Bs,A,0,S).

hamming_sum([],_,S,S).
hamming_sum([A|As],B,S1,S2):-
  hamming(A,B,D),
  S is S1+D,
  hamming_sum(As,B,S,S2).

% hamming distance between points

hamming(A,B,D):-bitxor(A,B,C),bitcount(C,D).

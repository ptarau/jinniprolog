bitcount(N,BitCount):-countbits(N,0,BitCount).

% entropy of a boolean function of NbOfBits args
% given as a 0..2^(2^NbOfBits)-1 number representing
% its truth table
tt_entropy(VarsCard,TT,Ones,TTLen,E):-
  exp2(VarsCard,TTLen),
  exp2(TTLen,Lim),MaxTT is Lim-1,
  TT>0,TT<MaxTT,
  bitcount(TT,Ones),
  bit_entropy(Ones,TTLen,E).

bit_entropy(Ones,TotalBits,E):-
  P1 is Ones/TotalBits,
  P0 is 1-P1,
  E is -(P1*log(2,P1)+P0*log(2,P0)).
  
countbits(0,S,S).
countbits(N,S1,S2):-
  N>0,B is N mod 2, 
  N1 is N//2,
  S is S1+B,
  countbits(N1,S,S2).
  
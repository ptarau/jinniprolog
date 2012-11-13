rp(Bits:N):-
  MBits is 1<<Bits,
  int2rbits(N,Bs0),rpad_bits_to(MBits,Bs0,Bs),
  println(Bits:Bs),nl,
  foreach(
    nth_member(B,Bs,I1),
    ( I is I1-1,
      int2rbits(I,Is0),
      rpad_bits_to(Bits,Is0,Is),
      println(tt(I,Is=>B))
    )
  ),
  nl.  

lpp(N):-int2bits(N,Bs),println(Bs).
rpp(N):-int2rbits(N,Bs),println(Bs).

tpp(NbOfBits,N):-
  [SP,C]=" :",
  L is 1<<NbOfBits, 
  int2lbits(L,N,Bs0),
  % reverse(Bs0,Bs),
  Bs=Bs0,
  write(N),put(C),
  foreach(
    nth_member(B,Bs,I),
    ( write(B),
      if(I mod 4=:=0,put(SP),true)
    )
  ),
  nl.
  

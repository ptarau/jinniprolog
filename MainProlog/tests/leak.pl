% fixed in 5.10
bug:-bug(20000).

bug(N):-
  findall(I-N,for(I,1,N),NNs),  sort(NNs,_),  fail.
bug(_):-  stat.

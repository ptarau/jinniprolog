:-[stack].

extendWith(Ys):-
  handle=>Xs,
  det_append(Xs,Ys,Zs), % very fast!
  handle<=Zs.

reverse:-
  handle=>Xs,
  reverse(Xs,Ys),
  handle<=Ys.

sort:-
  handle=>Xs,
  sort(Xs,Ys),
  handle<=Ys.

   
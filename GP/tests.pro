ktest:-
 N=3,
 var_to_bitstring(N,0,X0),   
 var_to_bitstring(N,1,X1),  
 var_to_bitstring(N,2,X2),  
 bitite(N,X0,X1,X2,R),
 tpp(N,X0),
 tpp(N,X1),
 tpp(N,X2),
 tpp(N,R).
 
ktest1:-
 N=4,
 var_to_bitstring(N,0,X1),   
 var_to_bitstring(N,1,X2),  
 var_to_bitstring(N,2,X3),   
 var_to_bitstring(N,3,X4),  
 bitxor(X1,X3,X5),
 bitxor(X1,X2,X6),
 bitxor(X3,X4,X7),
 bitor(X5,X6,X8),
 bitxor(X6,X7,X9),
 bitnot(N,X9,X9N),
 bitand(X8,X9N,X10),
 bitite(N,X1,X2,X3,R),
 bitite(N,X3,X2,X1,RR),
 tpp(N,X1),
 tpp(N,X2),
 tpp(N,X3),
 tpp(N,X4),
 nl,
 tpp(N,X5),
 tpp(N,X6),
 tpp(N,X7),
 tpp(N,X8),
 tpp(N,X9),
 tpp(N,X10),
 tpp(N,R),
 tpp(N,RR).

% tests

ldags:-ldags(4,9).

ldags(M,N):-
  for(IM,1,M),
    nl,
    for(IN,1,N),
      leafdags(IN,IM,R),
  println(R),fail
; nl.

/*
?- ldags.

1
2
5
14
42
132
429
1430
4862

4
16
80
448
2688
16896
109824
732160
4978688

9
54
405
3402
30618
288684
2814669
28146690
287096238

16
128
1280
14336
172032
2162688
28114944
374865920
5098176512
no
*/

neg(0,1).
neg(1,0).

conj(0,0,0).
conj(0,1,0).
conj(1,0,0).
conj(1,1,1).

less(0,0,0).
less(0,1,1).
less(1,0,0).
less(1,1,0).

mneg(X,Y,Rs,R):-findall(x(X,Y),neg(X,Y),Ns),sort(Ns,Rs),xblub(Rs,R).
mconj(X,Y,Z,Rs,R):-findall(x(X,Y,Z),conj(X,Y,Z),Cs),sort(Cs,Rs),xblub(Rs,R).
mless(X,Y,Z,Rs,R):-findall(x(X,Y,Z),less(X,Y,Z),Cs),sort(Cs,Rs),xblub(Rs,R).

mneg(X,Rs):-findall(Y,neg(X,Y),Ns),sort(Ns,Rs).
mconj(X,Y,Rs):-findall(Z,conj(X,Y,Z),Cs),sort(Cs,Rs).
mless(X,Y,Rs):-findall(Z,less(X,Y,Z),Cs),sort(Cs,Rs).

blub(0,0,0).
blub(0,1,_).
blub(1,0,_).
blub(1,1,1).

% lub of a list of terms
xblub([T|Ts],L):-map_blub(Ts,T,L).

map_blub([],L,L):-!.
map_blub([T|Ts],L1,L3):-
	fblub(L1,T,L2),
	map_blub(Ts,L2,L3).
	
fblub(F,G,H):-F=..[A|Fs],G=..[A|Gs],map(blub,Fs,Gs,Hs),H=..[A|Hs].

optable(Op,R):-
  member(X,[0,1,_]),member(Y,[0,1,_]),member(Z,[0,1,_]),
  call(Op,X,Y,Z,_,R).

find_unique(X,G):-findall(X,G,Xs),sort(Xs,Rs),member(X,Rs).

optable(Op):-  
  count_answers(optable(Op,_),Nb),
  count_answers(find_unique(R,optable(Op,R)),Un),
    println(answers=Un/Nb),
  find_unique(R,optable(Op,R)),
  pp_clause(Op:R),
  fail.
  
  
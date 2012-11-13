go:-go(3).

go(N):- go(N,_).

go(N,R):- once(g3(N,R)).  

g3(N,Xs):-for(_,1,N),findall(X,g2(N,X),Xs).

g2(N,Xs):-for(_,1,N),findall(X,g1(N,X),Xs).

g1(N,Xs):-for(_,1,N),findall(X,g0(N,X),Xs).

g0(N,Xs):-for(I,1,N),findall(I,for(I,1,N),Xs).

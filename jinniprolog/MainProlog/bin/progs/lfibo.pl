fibo(N,_,N).
fibo(N1,N2,N):-N3 is N1+N2,fibo(N2,N3,N).

f_test(K,N):-for(_,1,K),nfibo(N,_),fail.
f_test(_,_).

nfibo(N,R):-fibo(1,1,I),I>=N,!,R=I.

go:-
  K is 100*1000,
  N is 100*1000*1000,
  go(K,N),
  FK is K//5,
  FN is N*1.00001,
  go(FK,FN).
  
go(K,N):-
	statistics(runtime,_),
	statistics(global_stack,[H1,_]),
	statistics(trail,[TR1,_]),
	f_test(K,N),
	statistics(runtime,[_,T]),
	statistics(global_stack,[H2,_]),
	statistics(trail,[TR2,_]),
	H is H2-H1,TR is TR2-TR1,
	nfibo(N,R),
	write([time=T,heap=H,trail=TR,test(K,N),fibo(R)]),nl.

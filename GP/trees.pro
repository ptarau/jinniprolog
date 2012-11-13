% leaf dags

funtree(NbNodes,NbPIs,PIs:T):-
  length(PIs,NbPIs),
  bintree(NbNodes,Leaves,T),
  functions_from(Leaves,PIs).

% binary trees

bintree(NbNodes,Leaves,T):-btree(T,NbNodes-Leaves,0-[]).

btree(Leaf)-->bleaf(Leaf).
btree(t(L,R))-->bnode,btree(L),btree(R).

bleaf(Leaf,N-[Leaf|Ls],N-Ls).

bnode(N1-Ls,N-Ls):-N1>0,N is N1-1.

% binary trees no dcgs - leafs seen as vars
% Catalan numbers: 1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862, 16796
vntree(NbNodes,Leaves, T):-vntree(NbNodes,0, Leaves,[], T).

vntree(N,N,[V|Vs],Vs,v(V)).
vntree(N1,N3,Vs1,Vs3,t(_,L,R)):-
  N1>0,N is N1-1,
  vntree(N,N2,Vs1,Vs2,L),
  vntree(N2,N3,Vs2,Vs3,R).

% binary trees no all nodes seen as vars
% Catalan numbers: 1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862, 16796
vtree(M, T):-N is 2*M+1,length(Ns,N),vtree(Ns,[], T).

vtree([V|Vs],Vs,v(V)).
vtree([N|Vs1],Vs3,t(N,L,R)):-
  vtree(Vs1,Vs2,L),
  vtree(Vs2,Vs3,R).

bt(1,e).
bt(N,c(L,R)):-N>1,N1 is N-1,for(I,1,N1),J is N-I,bt(I,L),bt(J,R).

fbt(N,T):-cbt(N,T,[]).

cbt(1,e,Bs):-map(call,Bs).
cbt(N,c(L,R),Bs):-N>1,N1 is N-1,for(I,1,N1),J is N-I,cbt(I,L,[fbt(J,R)|Bs]).


% subset generator
subset_of([],[]).
subset_of([X|Xs],Zs):-
  subset_of(Xs,Ys),
  add_element(X,Ys,Zs).

add_element(_,Ys,Ys).
add_element(X,Ys,[X|Ys]).

% subsets of K elements
k_subset(0,_,[]).
k_subset(K,[X|Xs],[X|Rs]):-K>0,K1 is K-1,k_subset(K1,Xs,Rs).
k_subset(K,[_|Xs],Rs):-K>0,k_subset(K,Xs,Rs).

% powerset generator
all_subsets([],[[]]).
all_subsets([X|Xs],Zss):-
  all_subsets(Xs,Yss),
  extend_subsets(Yss,X,Zss).

extend_subsets([],_,[]).
extend_subsets([Ys|Yss],X,[Ys,[X|Ys]|Zss]):-extend_subsets(Yss,X,Zss).


% cartesian product of two sets
cproduct([],_,[]).
cproduct([X|Xs],Ys,NewPs):-
  cproduct1(Ys,X,NewPs,Ps),
  cproduct(Xs,Ys,Ps).

cproduct1([],_,Ps,Ps).
cproduct1([Y|Ys],X,[e(X,Y)|NewPs],Ps):-
  cproduct1(Ys,X,NewPs,Ps).


% graphs

graph(N,Gs):-
  nats(0,N,Ns),
  cproduct(Ns,Ns,Ps),
  subset_and_complement_of(Ps,Es,_),
  isolated_of(Ns,Es,Is),
  append(Is,Es,Gs).

isolated_of(Ns,Es,Is):-findall(I,an_isolated_of(Ns,Es,I),Is).
  
an_isolated_of(Ns,Es,i(N)):-
   member(N,Ns),
   \+member(e(N,_),Es),
   \+member(e(_,N),Es).

% subset + complement generator
subset_and_complement_of([],[],[]).
subset_and_complement_of([X|Xs],NewYs,NewZs):-
  subset_and_complement_of(Xs,Ys,Zs),
  place_element(X,Ys,Zs,NewYs,NewZs).

place_element(X,Ys,Zs,[X|Ys],Zs).
place_element(X,Ys,Zs,Ys,[X|Zs]).

all_k_subsets(0,_,[[]]).
all_k_subsets(K,[],[]):-K>0.
all_k_subsets(K,[X|Xs],Yss):-
  K>0,K1 is K-1,
  all_k_subsets(K1,Xs,Lss),
  all_k_subsets(K,Xs,Rss),
  mapcons(X,Lss,Rss,Yss).

% all integer partitions

all_ipartitions(N,Pss):-
  nats(1,N,Rs),
  reverse(Rs,Ks),
  sums(N,Ks,Pss).

sums(0,[],[[]]):-!.
sums(_,[],[]):- !.
sums(N,[K|Ks],R):-K > N,!,sums(N,Ks,R).
sums(N,[K|Ks],R):-N_K is N-K,
  sums(N_K,[K|Ks],RK),
  sums(N,Ks,End),
  mapcons(K,RK,End,R).

mapcons(_,[],End,End):-!.
mapcons(A,[Xs|Xss],End,[[A|Xs]|Yss]):-mapcons(A,Xss,End,Yss).

% backtracking integer partition iterator
 
ipartition_of(N,Ps):-
  nats(1,N,Is),
  reverse(Is,Ks),
  nsum(N,Ks,Ps).

nsum(0,_,[]).
nsum(N,[K|Ks],R):-
  N>0,
  nsum_choice(N,K,Ks,R).

nsum_choice(N,K,Ks,[K|R]):-
  NK is N-K,
  nsum(NK,[K|Ks],R).
nsum_choice(N,_,Ks,R):-
  nsum(N,Ks,R).

nats(Max,Max,[Max]).
nats(Curr,Max,[Curr|Ns]):-
	Curr<Max,
	Curr1 is Curr+1,
	nats(Curr1,Max,Ns).
	
% functions from N to M as a list of elements m=>n

funpairs_from([],_,[]).
funpairs_from([V|Vs],Us,[U=>V|Ps]):-member(U,Us),funpairs_from(Vs,Us,Ps).

arrow_to_funs(Is=>Js,As):-funpairs_from(Js,Is,As).

circuit_of(D,K,NI=>NO,NG,Rs):-
  lpartition_of(D,NI,NO,NG,Lss),
  reverse(Lss,Rss),
  rtree_from(Rss,K,Ns,[]),
  sort(Ns,Rs).
 
rtree_from([],_)-->[].
rtree_from([[_|_]],_)-->[].
rtree_from([[]|Xss],K)-->rtree_from(Xss,K).
rtree_from([[T|Ts],Fs|Xss],K)-->
  [Is=>T],
  {k_subset(K,Fs,Is)},
  rtree_from([Ts,Fs|Xss],K).

lpartition_of(D,NPI,NPO,NG,Iss):-
  N is NPI+NPO+NG,
  ipartition_of(NG,Ps),
  length(Ps,L),L=<D,
  append([NPI|Ps],[NPO],Qs),
  ndag(N,Qs,Iss).

ndag(_,[],[]).
ndag(Max,[N|Ns],[Is|Iss]):-
  sum(Ns,S),
  L is Max-(N+S),
  R is Max-(S+1),
  nats(L,R,Is),
  ndag(Max,Ns,Iss).

dgtest(N):-
  ctime(T1),count_answers(circuit_of(20,2,3=>1,N,_Gs),R),
  ctime(T2),T is T2-T1,
  println([circs=R,time=T]).
  
%%%%%%%%%%%%%%%%%%%% end %%%%%%%%%%%%%%%%%%%%%%%%5
/*
epairs(Iss,Es):-findall(E,epair(Iss,E),Es).

epair(Iss,Ts=>Fs):-  
  pref_of(Iss,Tss,Fs),
  appendN(Tss,Ts),
  Ts=[_|_].

pref_of(Is,Ps,P):-append(Ps,[P|_],Is).

suf_of(Is,S,Ss):-append(_,[S|Ss],Is).

subset2_of(Xs,[X,Y]):-suf_of(Xs,X,Ys),member(Y,Ys).

subset3_of(Xs,[X,Y,Z]):-suf_of(Xs,X,Ys),suf_of(Ys,Y,Zs),member(Z,Zs).
*/  

/*  
negate(0,1).
negate(1,0).

neglist([],[]).
neglist([X|Xs],[Y|Ys]):-negate(X,Y),neglist(Xs,Ys).

self_sim_step(Xs,Zs):-neglist(Xs,Ys),append(Xs,Ys,Zs).

self_sim(Xs,Zs):-self_sim_step(Xs,Ys),self_sim_next(Ys,Zs).

self_sim_next(Xs,Xs).
self_sim_next(Xs,Ys):-self_sim(Xs,Ys).
*/


/*
% circuits as DAGs with constraints: depth, arity, PIs,POs
circuit_of(D,Arity,NPI=>NPO,NG,Gs):-gates_of(D,Arity,NPI,NPO,NG,_Iss,Gs).

gates_of(D,Arity,NPI,NPO,NG,Iss,Gs):-
  epartition_of(D,NPI,NPO,NG,Iss,As),
  to_gates(As,Arity,Gs).

to_gates([],_,[]).
to_gates([Fs=>Ts|As],Arity,NewGss):-
  from_to2(Arity,Fs,Ts,Gs),
  to_gates(As,Arity,Gss),
  append(Gs,Gss,NewGss).
  
from_to2(Arity,Fs,Ts,IJOs):- 
  all_k_subsets(Arity,Fs,IJs),
  match_each_of(Ts,IJs,IJOs).

match_each_of([],_,[]).
match_each_of([O|Os],IJs,[IJ=>O|IJOs]):-
  sel(IJ,IJs,More),
  match_each_of(Os,More,IJOs).

%
epartition_of(D,NPI,NPO,NG,Iss,Es):-
  lpartition_of(D,NPI,NPO,NG,Iss),
  gpairs(Iss,Es).

gpairs(Xss,Zss):-
  reverse(Xss,Rss),
  map(reverse,Rss,Uss),
  xpairs(Uss,Yss),
  reverse(Yss,[_|Zss]).

xpairs([],[]).
xpairs([Xs|Xss],[Ys1=>Xs1|Rss]):-
  appendN(Xss,Ys),
  reverse(Ys,Ys1),
  reverse(Xs,Xs1),
  xpairs(Xss,Rss).

sel(X,[X|Xs],Xs).
sel(X,[Y|Xs],[Y|Ys]):-sel(X,Xs,Ys).
*/

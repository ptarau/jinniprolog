% terms

term2eqs(T,S,Rs):-term2eqs(T,S,Es,[]),sort(Es,Qs),equalize_keyed_list(Qs,Rs).

term2eqs(T,X)-->{var(T)},!,{T=X}.
term2eqs(T,X)-->{number(T);atom(T)},!,add_keyed_eq(T,X).
term2eqs(T,X)-->{compound(T)},!,{functor(T,F,N),functor(S,F,N)},add_keyed_eq(S,X),args2eqs(0,N, T,S).

args2eqs(N,N,_,_)-->[].
args2eqs(I,N,T,S)-->{I<N,I1 is I+1,arg(I1,T,A),arg(I1,S,X)},term2eqs(A,X),args2eqs(I1,N,T,S).

add_keyed_eq(Key,Val)-->[Key=Val].

equalize_keyed_list(Xs,Rs):-
  sort(Xs,Ys),
  equalized_keyed(Ys,Flag),
  ( var(Flag)->Rs=Ys
  ; equalize_keyed_list(Ys,Rs)
  ).

equalized_keyed([],_Flag).
equalized_keyed([X|Xs],Flag):-equalize_list(Xs,X,Flag).

equalize_list([],_,_).
equalize_list([K=X|Ps],K0=X0,Flag):-K==K0,!,X=X0,Flag=yes,equalize_list(Ps,K0=X0,Flag).
equalize_list([P|Ps],_,Flag):-equalize_list(Ps,P,Flag).

teq2cat(R:Ts,Cat):-
  new_cat(Cat),
  foreach(
    teq2pair(R:Ts,e(From,To,F,I)),
    ( 
      set_prop(Cat,From,'Color','Red'),
      set_prop(Cat,To,'Color','Red'),
      set_hyper(Cat,From,0),
      set_hyper(Cat,To,0),
      set_morphism_and_color(Cat,From,To,F,I)

    )
  ).
  
teq2pair(RT0,E):-
  copy_term(RT0,RT),numbervars(RT,0,_),RT=(_R:Ts),
  member(K=V,Ts),
    arg(1,V,To),
    ( (number(K);atomic(K))->E=e(K,To,x,0)
    ;
     functor(K,F,N),
     for(I,1,N),
      arg(I,K,VFrom),
        arg(1,VFrom,From),
        E=e(From,To,F,I)
     ).

show_teq(T):-
  term2eqs(T,S,Es),
  teq2cat(S:Es,C),
  foreach(teq2pair(S:Es,P),println(P)),
  pp_clause((T:-(S:Es))),
  rshow(C).
          
teq_test(T):-
  pp_clause(T),
  term2eqs(T,S,Es),pp_clause((S:-Es)),
  foreach(teq2pair(S:Es,P),println(P)),
  rshow(T),
  sleep(3),
  teq2cat(S:Es,C),
  rshow(C),
  dualize(C),
  sleep(3),
  rshow(C),
  fail.

teq:-
  teq_test(f(99,X,h(Y,u(3.1)),g(X,h(Y,u(3.1)),a,99),h(a),Y)).
  
% pairs of integers to Cats
ipairs2igraph(Edges,Isolated,Cat):-
  new_cat(Cat),
  ( member(V,Isolated),
    set_prop(Cat,V,"v","i"),
    fail
  ; member((From,To),Edges),
    set_morphism(Cat,From,To,"f","t"),
    fail
  ; true
  ).
  
igtest:-
  ipairs2igraph(
    [(0,1),(1,3),(3,4),(4,1)],
    [2,5,6],
  Cat),
  rshow(Cat), % shows as built
  igraph2bigint(Cat,B),
  bigint_print(B),nl,
  sleep(5),
  bigint2string(B,S),
  g(S). % shows again
  

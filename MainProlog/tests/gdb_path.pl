gassert(G,D,H,B):-
  gensym_no('$clause',ClauseNo),
  add_edge(G,'$clause',ClauseNo,(H:-B)),
  functor(H,F,N),
  add_edge(G,'$db',D,true),
  Pred='$pred'(D,F,N),
  add_edge(G,D,Pred,true),
  add_edge(G,Pred,ClauseNo,true),
  foreach(
    ground_const_of(H,XF,XN,I),
    add_edge(G,'$arg'(D,F,N,XF,XN,I),ClauseNo,true)
  ).

% ginstantiate(G,D,Pred):-
gclause(G,D,H,B):-
  edge_of(G,'$db',D),
  (functor(H,F,N)->true;edge_of(G,D,Pred)),
  Pred='$pred'(D,F,N),
  findall(Cs,matching_clause_of(G,D,H,F,N,Cs),Css),
  println(Css:H),
  ( intersectN(Css,Cs)->
    member(C,Cs)
  ; true
  ),
  edge_of(G,Pred,C),
  edge_of(G,'$clause',C,(H:-B)).

matching_clause_of(G,D,H,F,N,Cs):-
  Arg='$arg'(D,F,N,XF,XN,I),
  const_of(H,XF,XN,I),
  findall(ClauseNo,edge_of(G,Arg,ClauseNo),Cs).
     
intersectN([Is|Iss],Rs):-foldl(intersect,Is,Iss,Rs).
  
intersect(Qs,As,Xs):-findall(X,(member(X,Qs),member(X,As)),Xs).
  
% const_of(H,F,N,I):-argn(I,H,A),constant_of(A,F,N).
const_of(H,F,N,I):-argn(I,H,A),functor(A,F,N).     
% ground_const_of(H,F,N,I):-argn(I,H,A),ground(A),constant_of(A,F,N).
ground_const_of(H,F,N,I):-argn(I,H,A),functor(A,F,N).
  
constant_of(T,CF,CN):-functor(T,F,N),pick_constant_of(T,F,N,CF,CN).

pick_constant_of(T,F,N,F,N):-indexable_constant(T,F,N).
pick_constant_of(T,_F,N,CF,CN):-for(I,1,N),arg(I,T,A),constant_of(A,CF,CN).

indexable_constant([_|_],_,_):-!,fail.
indexable_constant([],_,_):-!,fail.
indexable_constant((_,_),_,_):-!,fail.
indexable_constant((_;_),_,_):-!,fail.
indexable_constant(_-_,_,_):-!,fail.
indexable_constant(T,_,_):-float(T),!,fail.
indexable_constant(_,_,_).


%%%%

gtest:-
  new_graph(G),
  init_gensym('$clause'),
  gassert(G,mydb,b(f(_),a),true),
  gassert(G,mydb,b(g(k),a),true),
  gassert(G,mydb,b(f(n),a),true),
  gassert(G,mydb,b(f(m),b),true),
 
  % show_graph(G),
  foreach(edge_of(G,From,To,D),println(From=>To:D)),
  
  % foreach(gclause(G,D,H,B),println(D:(H:-B))),
  
  foreach(gclause(G,_,b(f(A)),_),println(x=A)).
  
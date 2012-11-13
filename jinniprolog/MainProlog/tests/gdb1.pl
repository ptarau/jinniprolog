gassert(G,D,H,B):-
  functor(H,F,N),
  gassertN(G,D,F,N,H,B).

gassertN(G,D,F,N,H,B):-
  % if it has unindexed clauses,
  % for sake of completenes - forget it
  gassert0existing(G,D,F,N,H,B),
  !.
gassertN(G,D,F,N,H,B):-
  N>1,
  gassert2(G,D,F,N,H,B),
  !.
gassertN(G,D,F,N,H,B):-
  N=:=1,
  gassert1(G,D,F,N,H,B),
  !.
gassertN(G,D,F,N,H,B):-
  % if it cannot be indexed - from now on
  % keep it unindexed - just as a set of clause
  gassert0new(G,D,F,N,H,B).

gassert0new(G,D,F,N,H,B):-
  db_edge(G,D,F/N,GH),
  pred_edge(GH,'$any','$any',Q),
  queue_add(Q,(H:-B)).
  
gassert0existing(G,D,F,N,H,B):-
  db_edge(G,D,F/N,GH),
  edge_of(GH,'$any','$any',Q),
  queue_add(Q,(H:-B)).
    
gassert2(G,D,F,N,H,B):-  
  db_edge(G,D,F/N,GH),
  arg(1,H,X),functor(X,FX,NX),
  arg(2,H,Y),functor(Y,FY,NY),
  pred_edge(GH,FX/NX,FY/NY,Q),
  queue_add(Q,(H:-B)).

gassert1(G,D,F,N,H,B):-  
  db_edge(G,D,F/N,GH),
  arg(1,H,X),functor(X,FX,NX),
  pred_edge(GH,FX/NX,'$any',Q),
  queue_add(Q,(H:-B)).
  
db_edge(G,From,To,E):-edge_of(G,From,To,E),!.
db_edge(G,From,To,E):-new_graph(E),add_edge(G,From,To,E).

pred_edge(GH,From,To,Queue):-edge_of(GH,From,To,Queue),!.
pred_edge(GH,From,To,Queue):-queue_create(Queue),add_edge(GH,From,To,Queue).
  
  
gclause(G,D,H,B):-
  gclauses(G,D,H,Q),
  queue_list(Q,Cs),
  member((H:-B),Cs).

gclauses(G,D,H,Q):-
  (functor(H,F,N)->FN=F/N;true),
  edge_of(G,D,FN,GH),
  FN=F/N,functor(H,F,N),
  gclausesN(GH,H,N,Q).
  
gclausesN(GH,H,N,Q):-N>1, 
  arg(1,H,X),
  (functor(X,FX,NX)->VX=FX/NX;true),
  arg(2,H,Y),
  (functor(Y,FY,NY)->VY=FY/NY;true),
  edge_of(GH,VX,VY,Q).
gclausesN(GH,H,N,Q):-N=:=1, 
  arg(1,H,X),
  (functor(X,FX,NX)->VX=FX/NX;true),
  edge_of(GH,VX,'$any',Q).
gclausesN(GH,_,_,Q):- 
  edge_of(GH,'$any','$any',Q).    

%%%%

gtest:-
  new_graph(G),D=mydb,
  % H=b(f(Q),Y,M,N),
  gassert(G,D,c(99),true),
  gassert(G,D,a(f(_),a,X,X),true),
  gassert(G,D,b(g(k),a,X,f(X)),true),
  gassert(G,D,b(f(n),X,X,g(X)),true),
  gassert(G,D,b(f(m),b,f(X),g(X)),true),
  foreach(edge_of(G,D,P,GP),and(println(pred=P),show_graph(GP))),
  foreach(gclause(G,DD,H,B),println(DD:(H:-B))).
  
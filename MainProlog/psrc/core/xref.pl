xreftest:-call_graph_info('prolog.pl',undefined).

xref(File):-call_graph_info(File,_any).

undefs(File):-call_graph_info(File,undefined).

xref(File,Cat):-xref(File,Cat,_Info).

xref(File,Cat,Info):-
  call_graph_of(File,Cat),
  Classic=false,
  ranked_graph_info(Cat,Classic,Info).

call_graph_info(FName,Class):-
  call_graph_of(FName,G),
  ranked_graph_info(G,false,Info),
  println(file(FName,Info)),
  foreach(classify_in(G,V,Class),println(Class=V)),
  delete_java_object(G).

classify_in(G,V,Class):-
  vertex_of(G,V),
  in_degree(G,V,IN),
  out_degree(G,V,ON),
  V=F/N,functor(T,F,N),
  classify_pred(T,IN,ON,R),
  Class=R.

classify_pred(T,Type):-classify_pred(T,_IN,0,Type).

classify_pred(T,_IN,_ON,Type):-is_builtin(T),!,Type=builtin.
classify_pred('!',_IN,_ON,Type):-!,Type=builtin.
classify_pred(T,_IN,_ON,Type):-is_compiled(T),!,Type=compiled.
classify_pred(T,_IN,_ON,Type):-is_dynamic(T),!,Type=(dynamic).
classify_pred(_,_IN,0,Type):-!,Type=undefined.
classify_pred(_,0,_ON,Type):-!,Type=uncalled.

call_graph_of(F,G):-
  new_cat(G),
  visit_call_graph_of(F,G).

visit_call_graph_of(F,G):-  
  foreach(
    rclause_of(F,NewF,C),
    clause2graph(F,NewF,G,C)
  ).  

clause2graph(F,NewF,G,(H:-B)):-
  in_body(B,BX),
  ( BX=(M:P),atomic(M)->
    add_call_from_to(NewF,M,G,H,P),
    visit_call_graph_of(M,G)
  ; add_call_from_to(F,NewF,G,H,BX)
  ).
  
add_call_from_to(F,NewF,G,H,BX):-
  functor(H,FH,NH),
  functor(BX,FB,NB),
  % symcat(FH,NH,VH),symcat(FB,NB,VB),
  VH=FH/NH,VB=FB/NB,
  % namecat(FH,'/',NH,VH),namecat(FB,'/',NB,VB),
  set_morphism(G,VH,VB,call_in,NewF),
  set_morphism(G,F,NewF,included_in,true).
  
in_body(V,_):-var(V),!,fail.
in_body('$object'(_),_):-!,fail.
in_body((A,B),X):-!,(in_body(A,X);in_body(B,X)).
in_body((A;B),X):-!,(in_body(A,X);in_body(B,X)).
in_body((A->B),X):-!,(in_body(A,X);in_body(B,X)).
in_body(A,X):-meta_arg(A,B),nonvar(B),!,in_body(B,X).
in_body(A,A).

meta_arg(findall(_,G,_),G).
meta_arg(bagof(_,G,_),G).
meta_arg(setof(_,G,_),G).
meta_arg(not(G),G).
meta_arg(\+(G),G).
meta_arg(call(G),G).
meta_arg(topcall(G),G).
meta_arg(metacall(G),G).
meta_arg(new_engine(_,G,_),G).

rclause_of(F,NewF,C):-
  new_dict(D),
  rclause_of(F,D,NewF,C).
  
rclause_of(F,D,NewF,C):-
  dict_put(D,F,seen),
  clause_of(F,C0),
  ( C0=':-'(Fs)->nonvar(Fs),member(IF,Fs),dict_get(D,IF,'$null'),rclause_of(IF,D,NewF,C)
  ; NewF=F,C=C0
  ).
   
   
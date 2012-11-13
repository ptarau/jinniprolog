new_dict(D):-
  new_java_class('prolog.logic.ObjectDict',C),
  new_java_object(C,void,D).

new_dict(Size,D):-
  new_java_class('prolog.logic.ObjectDict',C),
  new_java_object(C,args(Size),D).

%$ returned value might consume symtable space !!!
dict_put(D,X,A):-nonvar(X),nonvar(A),!,invoke_java_method(D,put(X,A),_).
dict_put(D,X,A):-errmes(instantiation_error,dict_put(D,X,A)).

% creates, if needed, and returns a new dictionary associated to X in D
dict_ensure_child(D,X,Child):-nonvar(D),nonvar(X),!,invoke_java_method(D,child(X),Child).
dict_ensure_child(D,X,A):-errmes(instantiation_error,dict_ensure_child(D,X,A)).

dict_get(D,X,A):-nonvar(D),nonvar(X),!,invoke_java_method(D,get(X),A).
dict_get(D,X,A):-errmes(instantiation_error,dict_get(D,X,A)).

get_ordinal(D,X,I):-nonvar(D),nonvar(X),!,invoke_java_method(D,getOrdinal(X),I).
get_ordinal(D,X,I):-errmes(instantiation_error,get_ordinal(D,X,I)).

%$ returned value might consume symtable space !!!
dict_remove(D,X):-nonvar(D),nonvar(X),!,invoke_java_method(D,remove(X),_).
dict_remove(D,X):-errmes(instantiation_error,dict_remove(D,X)).

% generic !!!
size_of(D,S):-invoke_java_method(D,size,S).

% ObjectIterator API for ObjectDict - reusable for Graph, Cat etc.

key_iterator(D,I):-invoke_java_method(D,getKeys,I).

key_of(D,K):-key_iterator(D,I),iterator_element(I,K).

iterator_element(I,E):-
  repeat,
     ( has_next(I)->next(I,E)
       ; !,delete_java_object(I),fail % ???
     ).          
  
has_next(ObjectIterator):-
  invoke_java_method(ObjectIterator,hasNext,R),
  is_true(R).

next(ObjectIterator,X):-
  invoke_java_method(ObjectIterator,next,X).

alist2dict(KVs,D):-
  new_dict(D),
  foreach(member(K-V,KVs),dict_put(D,K,V)).

dict2alist(D,KVs):-findall(KV,dict2pair(D,KV),KVs).

dict2pair(D,K-V):-
   key_of(D,K),
   dict_get(D,K,V).
   
intersect_count(D1,D2, R):-invoke_java_method(D1,interesect_count(D2),R).
simdif_count(D1,D2, R):-invoke_java_method(D1,simdif_count(D2),R).
dif_count(D1,D2, R):-invoke_java_method(D1,dif_count(D2),R).
union_count(D1,D2, R):-invoke_java_method(D1,union_count(D2),R).

shared_in(D1,D2, Rs):-invoke_java_method(D1,intersect_with(D2),Rs).
unshared_in(D1,D2, Rs):-invoke_java_method(D1,difference_with(D2),Rs).
unshared(D1,D2, Rs):-invoke_java_method(D1,simdif_with(D2),Rs).

% graphs
  
new_graph(G):-
  new_java_class('prolog.core.Graph',C),
  new_java_object(C,void,G).

new_graph(Size,G):-
  new_java_class('prolog.core.Graph',C),
  new_java_object(C,args(Size),G).
    
add_vertex(G,V,D):-
  invoke_java_method(G,addVertex(V,D),_).
  
add_edge(G,From,To,Data):-
  invoke_java_method(G,addEdge(From,To,Data),_).
   
remove_edge(G,From,To):-
  invoke_java_method(G,removeEdge(From,To),R),
  is_true(R).

edge_count(G,N):-
  invoke_java_method(G,getEdgeCount,N).
   
remove_vertex(G,V):-
  invoke_java_method(G,removeVertex(V),R),
  is_true(R).   

is_vertex(G,V):-
  invoke_java_method(G,get(V),D),
  D\=='$null'.
  
vertex_data(G,V,D):-
  invoke_java_method(G,vertexData(V),D).

edge_data(G,From,To,D):-
  invoke_java_method(G,edgeData(From,To),D).

in_degree(G,V,N):-
  invoke_java_method(G,inDegree(V),N).

out_degree(G,V,N):-
  invoke_java_method(G,outDegree(V),N).

vertex_iterator(G,I):-
  invoke_java_method(G,vertexIterator,I).
  
in_iterator(G,V,I):-
  invoke_java_method(G,inIterator(V),I).

out_iterator(G,V,O):-
  invoke_java_method(G,outIterator(V),O).

vertex_of(G,V):-nonvar(V),!,is_vertex(G,V).
vertex_of(G,V):-  
  vertex_iterator(G,Vs),
  iterator_element(Vs,V).
     
incoming_of(G,V,IV):-  
  in_iterator(G,V,Vs),
  iterator_element(Vs,IV).

outgoing_of(G,V,OV):-  
  out_iterator(G,V,Vs),
  iterator_element(Vs,OV).
    
edge_of(G,From,To,D):-
  edge_of(G,From,To),
  edge_data(G,From,To,D),
  D\=='$null'.

back_edge_of(G,From,To,D):-edge_of(G,To,From,D).
  
edge_of(G,From,To):-nonvar(From),nonvar(To),!,is_vertex(G,From),is_vertex(G,To).
edge_of(G,From,To):-nonvar(From),!,is_vertex(G,From),outgoing_of(G,From,To).
edge_of(G,From,To):-nonvar(To),!,is_vertex(G,To),incoming_of(G,To,From).
edge_of(G,From,To):-vertex_of(G,From),outgoing_of(G,From,To).

show_graph(G,S):-object_to_string(G,S).

show_graph(G):-
  show_graph(G,S),
  println(S).

get_selected(G,V,D):-invoke_java_method(G,getSelected(V),D).

set_select_filter(G,Attr):-invoke_java_method(G,setSelectFilter(Attr),_).

% add graph based dynamic db API here

new_ranked_graph(G):-
  new_java_class('prolog.core.RankedGraph',C),
  new_java_object(C,void,G).

new_ranked_graph(Size,G):-
  new_java_class('prolog.core.RankedGraph',C),
  new_java_object(C,args(Size),G).
  
new_ranked_graph(G,D,E,Max):-
  new_java_class('prolog.core.RankedGraph',C),
  new_java_object(C,args(D,E,Max),G).

new_ranked_graph(Size,G,D,E,Max):-
  new_java_class('prolog.core.RankedGraph',C),
  new_java_object(C,args(Size,D,E,Max),G).


random_ranked_graph(G,Info):-
  random_ranked_graph(20,4,G,Info).
  
random_ranked_graph(Size,Degree,G,Info):-
  new_ranked_graph(G),
  randomize_graph(G,Size,Degree,Info).

random_ranked_graph(Seed,Size,Degree,G,Times,MaxRank,CompCount,Giant,GSize):-
  new_ranked_graph(G),
  randomize_graph(G,Seed,Size,Degree,Times,MaxRank,CompCount,Giant,GSize).


random_cat(G,Info):-
  random_cat(20,4,G,Info).
  
random_cat(Size,Degree,G,Info):-
  new_cat(G),
  randomize_graph(G,Size,Degree,Info).

random_cat(Seed,Size,Degree,G,Times,MaxRank,CompCount,Giant,GSize):-
  new_cat(G),
  randomize_graph(G,Seed,Size,Degree,Times,MaxRank,CompCount,Giant,GSize).

ranked_graph_info(G):-
  ranked_graph_info(G,true,Info),
  println(Info).

ranked_graph_info(G,How,
  [ vertices=VN,edges=EN,pr_steps=Times,maxrank=MaxRank,
    compcount=CompCount,giant=Giant,giantsize=GSize
  ]):- 
  size_of(G,VN),
  edge_count(G,EN),
  to_boolean(How,Classic),
  ranked_graph_info0(G,Classic,Times,MaxRank,CompCount,Giant,GSize).
  
ranked_graph_info0(G,Classic,Times,MaxRank,CompCount,Giant,GSize):- 
  mark_components(G),
  count_components(G,CompCount),
  giant_component(G,Giant),
  component_size(G,Giant,GSize),
  rank_graph(G,Classic,Times),
  rank_sort(G),
  vertex_iterator(G,I),
  next(I,High),
  get_rank(G,High,MaxRank).

trim_ranked_graph(RG,Giant,Max,TG):-
  invoke_java_method(RG,trimRankedGraph(Giant,Max),TG).

rank_graph(G,Classic,Times):-
  invoke_java_method(G,runGraphRanker(Classic),Times).
      
rank_graph(G,Times):-
  invoke_java_method(G,runGraphRanker,Times).

rank_graph(G):-
  rank_graph(G,Times),
  println(page_rank_steps=Times).

rank_sort(G):-invoke_java_method(G,rankSort,_).
    
get_rank(G,V,R):-
  invoke_java_method(G,getRank(V),R).

set_rank(G,V,R):-
  invoke_java_method(G,setRank(V,R),_).

get_hyper(G,V,R):-
  invoke_java_method(G,getHyper(V),R).

set_hyper(G,V,R):-
  invoke_java_method(G,setHyper(V,R),_).

mark_components(G):-
  invoke_java_method(G,markComponents,_).

mark_components(G,HasCycle):-
  invoke_java_method(G,checkCycle,HasCycle).
    
get_component(G,V,R):-
  invoke_java_method(G,getComponent(V),R).

set_component(G,V,R):-
  invoke_java_method(G,setComponent(V,R),_).
  
count_components(G,N):-
  invoke_java_method(G,countComponents,N).

giant_component(G,N):-
  invoke_java_method(G,giantComponent,N).

component_size(G,N,Size):-
  invoke_java_method(G,componentSize(N),Size).
   
filter_component(G,I,ObjectStack):-
  invoke_java_method(G,filterComponent(I),ObjectStack).
    
new_cat(C):-
  new_java_class('prolog.core.Cat',Cat),
  new_java_object(Cat,void,C).

new_cat(Size,C):-
  new_java_class('prolog.core.Cat',Cat),
  new_java_object(Cat,args(Size),C).
  
set_prop(C,V,X,A):-
 invoke_java_method(C,setProp(V,X,A),_R).

get_prop(C,V,X,R):-
 invoke_java_method(C,getProp(V,X),R).

prop_of(C,V,X,R):-nonvar(V),nonvar(X),!,get_prop(C,V,X,R).
prop_of(C,V,X,R):-
  vertex_of(C,V),  
  invoke_java_method(C,propIterator(V),Xs),
  iterator_element(Xs,X),
  get_prop(C,V,X,R).
  
set_morphism(C,F,T,M,D):-
  invoke_java_method(C,setMorphism(F,T,M,D),_R).

get_morphism(C,F,T,M,D):-
  invoke_java_method(C,getMorphism(F,T,M),D).

morphism_of(C,F,T,M,D):-nonvar(F),nonvar(T),nonvar(M),!,get_morphism(C,F,T,M,D).
morphism_of(C,F,T,M,D):-edge_of(C,F,T),  
  invoke_java_method(C,morphismIterator(F,T),Ms),
  iterator_element(Ms,M),
  morphism_of(C,F,T,M,D).
    
remove_morphism(C,F,T,M):-
  invoke_java_method(C,removeMorphism(F,T,M),_R).

term_to_hyper(T,C):-call_java_class_method('prolog.core.Cat',fromTerm(T),C).

term_to_cat(T,C):-call_java_class_method('prolog.core.Cat',termTree(T),C).

term_to_cat(T,FType,CType,C):-call_java_class_method('prolog.core.Cat',termTree(T,FType,CType),C).

new_small_cat(C):-
  new_java_class('prolog.core.SmallCat',Cat),
  new_java_object(Cat,void,C).

define_morphism(C,F,T,M):-
  invoke_java_method(C,defineMorphism(F,T,M),_R).

set_morphism(C,F,T,M):-
  invoke_java_method(C,setMorphism(F,T,M),_R).
    
add_morphism(C,F,T,M):-
  invoke_java_method(C,addMorphism(F,T,M),_R).

clear_morphism(C,F,T,M):-
  invoke_java_method(C,clearMorphism(F,T,M),_R).

rancat(Cat):-new_cat(Cat),randomize_graph(Cat,_).

randomize_graph(Graph,Info):-
  randomize_graph(Graph,20,4,Info).
  
randomize_graph(Graph,Size,Degree,Info):-
  randomize_graph(Graph,0,Size,Degree,Times,MaxRank,Count,Giant,GSize),
  Info=[ 
    size=Size,maxdeg=Degree,pr_steps=Times,max_rank=MaxRank,
    comps=Count,giant=Giant,giant_size=GSize
  ].

randomize_graph(Graph,Seed,Size,Degree,Times,MaxRank,CompCount,Giant,GSize):-
  Size>0,Degree>1,
  invoke_java_method(Graph,randomize(Seed,Size,Degree),_),
  to_boolean(true,Classic),
  ranked_graph_info0(Graph,Classic,Times,MaxRank,CompCount,Giant,GSize).
  
add_hyper_edge(RG,Name,Keys):-
  list_array(Keys,A),
  invoke_java_method(RG,addHyperEdge(Name,A),_),
  delete_java_object(A).

dualize(RG):-invoke_java_method(RG,dualize,_).

dualize_hyper(RG):-invoke_java_method(RG,dualizeHyper,_).

add_hyper_arrow(RG,Name,Is,Os):-
  list_array(Is,From),
  list_array(Os,To),
  invoke_java_method(RG,addHyperArrow(Name,From,To),_),
  delete_java_object(From),
  delete_java_object(To).

to_xml(Cat,FileName):-
  invoke_java_method(Cat,to_xml(FileName),_).

/* exmple:
?- rancat(C),new_cat(N),(cat_to_prolog(C,N:P),P,fail;true),cat_to_prolog(N,NP).
*/

cat_to_prolog(C,CatVar:Cmd):-
  prop_of(C,V,X,R),
  Cmd=set_prop(CatVar,V,X,R).
cat_to_prolog(C,CatVar:Cmd):-
  morphism_of(C,F,T,M,D),
  Cmd=set_morphism(CatVar,F,T,M,D).

pp_cat(C):-
    prop_of(C,V,X,R),
    map(unwrap_object,[V,X,R],[VV,XX,RR]),
    pp_clause(VV:XX=RR),
  fail.
pp_cat(C):-
    morphism_of(C,F,T,M,D),
    map(unwrap_object,[F,T,M,D],[FF,TT,MM,DD]),
    pp_clause(FF=>TT:MM=DD),
  fail.
pp_cat(_C):-nl.

unwrap_object(O,S):-nonvar(O),functor(O,'$object',1),!,object_to_string(O,S).
unwrap_object(T,T).

rgtest:-
  call_java_class_method('prolog.core.RankedGraph',rgtest,_).

/*
srgtest:-
  call_java_class_method('prolog.core.RankedGraph',srgtest,_).
  
gtest:-
  call_java_class_method('prolog.core.Graph',gtest,_).
    
hgtest:-
  println('JAVA'),
  call_java_class_method('prolog.core.RankedGraph',hgtest,_),
  nl,
  println('PROLOG'),
  new_ranked_graph(G),
  add_hyper_edge(G,'AB',['A','B']),
  add_hyper_edge(G,'BC',['B','C']),
  add_hyper_edge(G,'CA',['C','A']),
  show_graph(G).

dhgtest:-
  new_ranked_graph(G),
  add_hyper_arrow(G,h1,[a],[c,d]),
  add_hyper_arrow(G,h2,[a,c],[a,b,c]),
  add_hyper_arrow(G,h3,[c,b],[d]),
  show_graph(G).
    
rgtest1:-
  D is 85/100, E is 1/1000, Max is 100,
  new_ranked_graph(G,D,E,Max),
  add_edge(G,a(9),b,111),
  add_edge(G,a(9),c,222),
  add_edge(G,c,a(9),333),
  add_edge(G,b,b,444),
  add_edge(G,b,d,555),
  add_edge(G,d,a(9),666),
  add_vertex(G,e(a),777),
  add_edge(G,a(9),d,888),
  % gtest1(G),
  show_graph(G),
  rank_graph(G,Times),
  println('graphRanker runs'=Times),
  show_graph(G),
  rank_sort(G),
  remove_edge(G,c,a(9)),
  % show_graph(G),
  remove_vertex(G,b),
  println('c=>a,b=>*,removed'=Times),
  show_graph(G).
  
gtest1(G):-
  println('!!!starting vertex_iteration'),
  vertex_iterator(G,Vs),
  repeat,
    ( has_next(Vs)->next(Vs,V),println(V),
      gtest2(G,V),
      fail
    ; !
    ),
  println('!!!end vertex_iteration').
    
gtest2(G,V):-
  in_degree(G,V,In),out_degree(G,V,Out),
  println(in(In)+out(Out)),
  in_iterator(G,V,Is),
  repeat,
    ( has_next(Is)->next(Is,I),println(in:I),
      fail
    ; !
    ),
  out_iterator(G,V,Os),
  repeat,
    ( has_next(Os)->next(Os,O),println(out:O),
      fail
    ; !
    ).
    
gbug1:-
  queue_create(H),
  handle2object(H,Q),
  new_ranked_graph(G),
  add_vertex(G,a/3,Q),
  vertex_data(G,a/3,D),
  println(Q=D).
  
gbug:-
  Q=d,
  new_ranked_graph(G),
  add_vertex(G,a,Q),
  add_vertex(G,b,Q),
  vertex_data(G,a,D),
  show_graph(G),
  println(Q=D).

jgtest:-
  F='temp.jc',
  new_ranked_graph(G),
  add_vertex(G,a,'v1'),
  add_vertex(G,b,'v2'),
  add_vertex(G,c,'v3'),
  add_vertex(G,d,'v3'),
  add_edge(G,'a','b','e1'),
  add_edge(G,'a','c','e2'),
  add_edge(G,'b','c','e3'),
  add_edge(G,'c','a','e4'),
  add_edge(G,'d','c','e5'),
  rank_graph(G),
  rank_sort(G),
  show_graph(G),
  object2file(G,F),
  println(saved(F)),
  file2object(F,H),
  println(load(F)),
  show_graph(H).
*/

ctest1:-
  new_cat(10,C),
  set_morphism(C,a,b,ab1,1),
  set_morphism(C,a,b,ab2,2),
  set_morphism(C,b,c,bc1,3),
  set_morphism(C,b,c,bc2,4),
  set_morphism(C,c,b,cb1,5),
  set_morphism(C,c,a,ca1,6),
  set_prop(C,b,age,42),
  X is 1/4,set_prop(C,b,rank,X),
  Y is 3/4,set_prop(C,c,rank,Y),
  show_graph(C),
  get_prop(C,b,age,AB),
  println(prop(b,AB)),
  get_morphism(C,c,a,ca1,M),
  println(morphism(c,a,ca1,M)),
  size_of(C,S),
  edge_count(C,E),
  println(size(S)+edge_count(E)),
  get_ordinal(C,b,O),
  println(ordinal(c)=O),
  to_xml(C,'temp.xml'),
  PG=prop_of(C,_V,_X,_R),
  foreach(PG,println(PG)),
  PE=morphism_of(C,_F,_T,_M,_D),
  foreach(PE,println(PE)),
  to_xml(C,'temp.xml'),
  xml_parser:to_cat('temp.xml',C1),
  to_xml(C1,'temp1.xml'),
  println('compare temp.xml and temp1.xml').
  

% ctest:-call_java_class_method('prolog.core.Cat',ctest,_).

rctest:-
  new_cat(C),
  randomize_graph(C,Info),
  println(Info).

sctest:-
  call_java_class_method('prolog.core.SmallCat',sctest,_).  

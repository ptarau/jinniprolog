/* Simple algorithm for computing the connected
   components of a directed graph
*/

% directed graph data

arc(a,b).
arc(b,c).
arc(c,a).
arc(d,d).
arc(d,e).
arc(e,d).
arc(f,f).

/* collects the connected components of a 
   directed graph represented by facts arc/2 as 
   a list of node lists 
   - main predicate -
*/
components(Css):-
  get_nodes(Ns),
  collect_components(Ns,[],Css).

/* collects all nodes */

get_nodes(TrimmedNs):-
  findall(N,(arc(N,_);arc(_,N)),Ns),
  sort(Ns,TrimmedNs).

/* finds a Y that is reachable from X */
  
reachable(X,Y):-reachable_from(X,Y,[]).

reachable_from(X,Z,DejaVues):-
  arc(X,Y),
  \+(member(Y,DejaVues)),
  ( Z=Y
  ; reachable_from(Y,Z,[Y|DejaVues])
  ).

/*
   collects all connected components 
   for a list of nodes (Css is initally empty)
*/
collect_components([],Css,Css).
collect_components([N|Ns],Css,NewCss):-
  component_of(N,Cs),
  findall(X,
   (member(X,Ns), \+(member(X,Cs))),
  NewNs),
  collect_components(NewNs,[Cs|Css],NewCss).
  
/*
  collects the connected component 
  reachable from node N
*/
component_of(N,Trimmed):-
  findall(C,reachable(N,C),Cs),
  sort(Cs,Trimmed).

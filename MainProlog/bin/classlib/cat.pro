:-[java_object].

/*
  Wrapper for Cat objects - implementing finite categories - 
  roughly equivalent to directed graphs with attributes 
  on vertices and edges (attribute/values on edges are seen 
  as morphisms between vertices).
*/

cat:-
  new_cat(Cat),
  object<=Cat.

cat(Term):-
  term_to_cat(Term,Cat),
  object<=Cat.
     
set_prop(Node,Attr,Val):-
  object=>Cat,
  set_prop(Cat,Node,Attr,Val).

get_prop(Node,Attr,Val):-
  object=>Cat,
  prop_of(Cat,Node,Attr,Val).

set_link(From,To,Attr,Val):-
  object=>Cat,
  set_morphism(Cat,From,To,Attr,Val).

get_link(From,To,Attr,Val):-
  object=>Cat,
  morphism_of(Cat,From,To,Attr,Val).

remove_link(From,To,Attr):-
  object=>Cat,
  remove_morphism(Cat,From,To,Attr).

add_random(Size,Degree, Info):-
  object=>Cat,
  randomize_graph(Cat,Size,Degree,Info).
  
to_xml(FileName):-
  object=>Cat,
  to_xml(Cat,FileName).

to_list(Props:Links):-
  findall(Node:Attr=Val,get_prop(Node,Attr,Val),Props),
  findall(From=>To:Attr=Val,get_link(From,To,Attr,Val),Links).


/*
% example of use:

?- new(cat,C),C:add_random(4,10,Info),C:to_list(R).
>>>: ??? normalizing ranks
C = '$instance'('$object'(1557),2)
Info = [size = 4,maxdeg = 10,pr_steps = 6,max_rank = 1.0,comps = 1,giant = 0,gia
nt_size = 4]
R = ['0' : (data = 'V'),'3' : (data = 'V'),'2' : (data = 'V'),'1' : (data = 'V')
] : [('3' => '0') : (data = 'E'),('2' => '0') : (data = 'E'),('2' => '2') : (dat
a = 'E'),('1' => '3') : (data = 'E')]
;
no
*/
  
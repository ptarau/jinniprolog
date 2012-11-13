init_ops:-get_global_prop(ops,initialized),!.
init_ops:-
  set_global_prop(ops,initialized),
  default_op(Pri,Assoc,Name),
  op(Pri,Assoc,Name),
  fail.
init_ops. 

% operator management

/*
%op(X,Y,Z):-call_ifdef(p_op(X,Y,Z),ttyprint(ignoring(op(X,Y,Z)))).
%current_op(X,Y,Z):-call_ifdef(p_current_op(X,Y,Z),(ttyprint(ignoring(current_op(X,Y,Z))),fail)).
*/

/*
plet(K,L,V):-let(K,L,V).
pval(K,L,V):-val(K,L,V).
*/

plet(K,L,V):-set_local_prop(K,L,V).
pval(K,L,V):-get_local_prop(K,L,V).

op(Pri,Assoc,Name):-op0(Name,Assoc,Pri).

op0(Name,Assoc,Pri):-
  gensym_no(op,OpCount),
  op_type(Assoc,Cls),
  plet(Cls,Name,Assoc),
  plet(Name,Cls,Pri),
  plet(Name,opmark,OpCount),
  plet(opmark,OpCount,Name).
  
op_type(xfy,infixop).
op_type(xfx,infixop).
op_type(yfx,infixop).

op_type(fx,prefixop).
op_type(fy,prefixop).

op_type(xf,postfixop).
op_type(yf,postfixop).

get_op(Name,Assoc,Pri):- % nonvar(Name),
	op_type(Assoc,Cls),
	get_op0(Name,Cls,Assoc,Pri).

get_op0(Name,Cls, Assoc,Pri):- % nonvar(Name), nonvar(Cls),
  Name \== '$null',
	pval(Cls,Name,Assoc),
	pval(Name,Cls,Pri),
	Pri>0.

current_op(Pri,Assoc,Name):-nonvar(Name),nonvar(Assoc),!,
  get_op(Name,Assoc,Pri).
current_op(Pri,Assoc,Name):-
  for(I,1,10000),
  (pval(opmark,I,Name)->get_op(Name,Assoc,Pri);!,fail).
  
default_op(1000,xfy,',').
default_op(1100,xfy,(';')).

default_op(1200,xfx,('-->')).
default_op(1200,xfx,(':-')).
default_op(1200,fx,(':-')).
default_op(700,xfx,'is').
default_op(700,xfx,'=').

default_op(1050,xfx,('@@')).

default_op(500,yfx,'-').
default_op(200,fy,'-').

default_op(500,yfx,'+').
default_op(200,fy,'+').

default_op(400,yfx,'/').
default_op(400,yfx,'*').
default_op(400,fx,'*').
default_op(400,yfx,(mod)).
default_op(200,yfx,('**')).
default_op(200,xfy,(^)).

default_op(300,fy,('~')).
default_op(650,xfy,'.').
default_op(660,xfy,'++').

default_op(700,xfx,'>=').
default_op(700,xfx,'>').
default_op(700,xfx,'=<').
default_op(700,xfx,(<)).
default_op(700,xfx,(=\=)).
default_op(700,xfx,(=:=)).

default_op(400,yfx,('>>')).
default_op(400,yfx,('<<')).
default_op(400,yfx,('//')).

default_op(200,yfx,('\/')).
default_op(200,yfx,('/\')).
default_op(200,yfx,('\')).
default_op(200,fx,('\')).

default_op(700,xfx,('@>=')).
default_op(700,xfx,('@=<')).
default_op(700,xfx,('@>')).
default_op(700,xfx,('@<')).

default_op(700,xfx,('\==')).
default_op(700,xfx,('==')).
default_op(700,xfx,('=..')).
default_op(700,xfx,('\=')).

default_op(900,fy,(not)).
default_op(900,fy,('\+')).
default_op(900,fx,(spy)).
default_op(900,fx,(nospy)).

default_op(950,fx,('##')).

default_op(950,xfy,('=>')).
default_op(950,xfx,('<=')).

default_op(1050,xfy,('->')).

default_op(850,fx,(dynamic)).
default_op(850,fx,(public)).
default_op(850,fx,(module)).
default_op(850,fx,(mode)).
default_op(850,fx,(multifile)).
default_op(850,fx,(discontiguous)).

default_op(1200,xfx,('::-')).

default_op(50,yfx,(':')).

% default_op(1200,fx,('?-')).

default_op(100,fx,('@')).

default_op(25,xfx,('@')).

default_op(200,fx,('?')).

default_op(50,fx,(^)).

default_op(500,fx,('#>')).
default_op(500,fx,('#<')).
default_op(500,fx,('#:')).
default_op(500,fx,('#+')).
default_op(500,fx,('#*')).
default_op(500,fx,('#=')).
default_op(500,fx,('#-')).
default_op(500,fx,('#?')).

default_op(0,fx,(dynamic)).
default_op(0,fx,(mode)).
default_op(0,fx,(module)).
default_op(0,fx,(public)).
default_op(0,fx,(memo)).
default_op(0,fx,(type)).
default_op(0,fx,(delphi)).
default_op(0,fx,(mod)).
default_op(0,fx,(extends)).
default_op(0,fx,(with)).

default_op(800,xfy,(':')).
default_op(700,xfx,('#=>')).
default_op(700,xfx,('<=#')).
default_op(700,xfx,('<==')).
default_op(700,xfx,('==>')).
default_op(700,xfx,(is_bigint)).
default_op(700,xfx,(is_bigdec)).

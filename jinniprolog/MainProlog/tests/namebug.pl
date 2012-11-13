go0:-
  name('dp',Ys),  
  name('ddp',Zs),
  append(Xs,Ys,Zs),
  println(Xs+Ys),
fail.

  
go1:-
  name('dp',Ys),  
  name('ddp',Zs),
  unappend(Zs,Xs,Ys),
  println(Xs+Ys),
fail.

unappend(Ys,[],Ys).
unappend([X|Zs],[X|Xs],Ys):-unappend(Zs,Xs,Ys).

ok:-
  name('dp',Ys0),copy_term(Ys0,Ys),
  name('ddp',Zs0),copy_term(Zs0,Zs),
  append1(Xs,Ys,Zs),
  println(Xs+Ys),
fail.

go:-
  name('dp',Ys),
  name('ddp',Zs),
  append1(Xs,Ys,Zs),
  println(Xs+Ys),
fail.

append1([],Ys,Ys).
append1([X|Xs],Ys,XZs):-XZs=[X|Zs],println(after(X,Zs)),append1(Xs,Ys,Zs).

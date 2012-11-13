% oo.pl

/*
 DESIGN NOTES:

 see UserGuide
*/

enter_instance(ClassName):-new(ClassName,Instance),enter_class(Instance).

visit_class(ClassName):-enter_class(ClassName),remove_prolog_class(ClassName).

enter_class(ClassOrInstance):-(ClassOrInstance:toplevel),fail;true.

exit:-stop.

get_prolog_class(ClassName,PrologClass):-
  % println(entering_get_prolog_class+ClassName),
  jhandle(get_prolog_class,ClassName,Handle),
  !,
  handle2object(Handle,PrologClass).
get_prolog_class(_ClassName,'$null').

/* 
get_prolog_class(ClassName,PrologClass):-
  % println(entering_get_prolog_class+ClassName),
  this_class_object(Prolog),
  invoke_java_method(
    Prolog,
    'getPrologClass',
    args(ClassName),
    PrologClass
  ).
*/

remove_prolog_class(ClassName):-
  this_class_object(Prolog),
  invoke_java_method(
    Prolog,
    'removePrologClass',
    args(ClassName),
    _). 
  
% this triggers ocompile ###
new_prolog_class(ClassName,PrologClass):-
  new_prolog_class(ClassName,ocompile,PrologClass).

new_prolog_class(ClassFileName,InitCall,PrologClass):-
  new_prolog_class(ClassFileName,ClassFileName,InitCall,PrologClass).
  
new_prolog_class(ClassName,FileName,InitCall,PrologClass):-
  do_unless_done(
    ClassName,
    try_new_prolog_class(ClassName,FileName,InitCall,PrologClass),
    true
  ).
  
try_new_prolog_class(ClassName,FileName,InitCall,PrologClass):-
  % println(entering+ClassName+FileName),
  get_prolog_class(ClassName,Existing),
  ( Existing='$null'->
     (find_file(FileName,AbsFile)->true;errmes(no_file_for_class(ClassName),FileName)),
     compile_class_code(ClassName,AbsFile,InitCall, PrologClass)
  ; PrologClass=Existing
  ).

compile_class_code(ClassName,AbsFile,InitCall, PrologClass):-
   new_code(ClassName,PrologClass),
   % typically InitCall= ocompile ##
   (get_global_prop(class_check,yes)->
      run_prolog_method(PrologClass,ocheck(ClassName))
    ; true
   ),
   run_prolog_method(PrologClass,call(InitCall,AbsFile)).
      
Instance:Goal :- 
  nonvar(Instance),
  Instance='$instance'(PrologClass,Id),
  !,
  call_prolog_method(PrologClass,call_in_instance(Id,Goal)).
ClassName:Goal:-
  atom(ClassName),
  !,
  new_prolog_class(ClassName,Code),
  call_prolog_method(Code,Goal).
PrologClass:Goal:-
  handle2object(_,PrologClass),
  !,
  call_prolog_method(PrologClass,Goal).
Bad:Goal:-
  errmes('bad_class_or_instance'(Bad),calling(Goal)).
   
new_class_engine(PrologClass,X,G,Solver):-
  handle2object(ClassHandle,PrologClass),
  new_engine(ClassHandle,X,G,Solver).
  
call_prolog_method(PrologClass,G0):-
  G=method_call(G0),
  new_class_engine(PrologClass,G,G,Solver),
  good_element_of(Solver,X),
  X=G.

method_call(Goal):-call_ifdef(delegate_call(Goal),Goal).

run_prolog_method(PrologClass,G):-
  call_once_prolog_method(PrologClass,_,G,_).
  
call_once_prolog_method(PrologClass,X,G,Answer):-
  new_class_engine(PrologClass,X,G,Solver),
  get(Solver,Result),
  stop(Solver),
  Answer=Result.

/* provides empty constructor for default class */

new(Instance):-
  this_class(Handle),
  handle2object(Handle,PrologClass),
  class_name(Constructor),
  make_instance(PrologClass,Constructor,Instance).
  
/*
  loads the class code on the first use
  and reuses it for other instances
  use renew/2 to refresh the class code
*/  
new(ClassNameAndArgs,Instance):-
  functor(ClassNameAndArgs,ClassName,_),
  new_prolog_class(ClassName,PrologClass),
  make_instance(PrologClass,ClassNameAndArgs,Instance). 

/*
  forces reload the class code
*/
renew(ClassNameAndArgs,Instance):-
  functor(ClassNameAndArgs,ClassName,_),
  remove_prolog_class(ClassName),
  new(ClassNameAndArgs,Instance).
  
call_in_instance(Instance,Goal):-
  set_instance_id(Instance),
  topcall(Goal).
   
make_instance(PrologClass,Constructor,Instance):-
  Instance='$instance'(PrologClass,Id),
  % invoke_java_method(PrologClass,'new_instance_id',void,Id),
  handle2object(Handle,PrologClass),
  new_instance_id(Handle,Id),
  Instance:call_ifdef('$super',true),
  (Constructor==prolog->true
  ;Instance:if_some(Constructor,true,errmes(failed_constructor,Constructor))
  ).

this(Instance):-
  Instance='$instance'(PrologClass,Id),
  this_class(Handle),
  handle2object(Handle,PrologClass),
  get_instance_id(Id).

/* gets the Prolog class associated to this Engine */

this_class_object(Object):-this_class(This),handle2object(This,Object).

handle2object(Handle,O):-functor(O,'$object',1),arg(1,O,Handle),integer(Handle).

handle2string(Handle,S):-
  this_class_object(Prolog),
  invoke_java_method(Prolog,handle2string(Handle),S).

/* backtrackable field operations */

F<=#V:-uput(F,V).
F#=>V:-uget(F,V).

/* class fields */

F<==V :-get_class_db(Db),set_field(Db,F,V).
F==>V :-get_class_db(Db),get_field(Db,F,V).

/* instance fields */
F<=V :- this_db(Db),set_field(Db,F,V).
F=>V :- this_db(Db),get_field(Db,F,V).

set_field(Db,F,X):-var(Db),!,errmes('set field: all args should be nonvar',set_field(Db,F,X)).
set_field(Db,F,X):-var(F),!,errmes('set field: all args should be nonvar',set_field(Db,F,X)).
set_field(Db,F,X):-var(X),!,errmes('set field: all args should be nonvar',set_field(Db,F,X)).
set_field(Db,F,X):-add_field(Db,F),let(Db,F,X).

get_field(Db,F,V):-var(Db),!,errmes('get field: keys should be nonvar',get_field(Db,F,V)).
get_field(Db,F,V):-var(F), !,errmes('get field: keys should be nonvar',get_field(Db,F,V)).
get_field(Db,F,V):-val(Db,F,X),!,V=X.
get_field(Db,F,_):-class_name(C),errmes(reference_to_uninitialised_field,C:F-Db).

add_field(Db,F):-val(Db,F,_),!.
add_field(Db,F):-db_assert(Db,'$field'(F)).

get_fields(Fs):-
  is_dynamic('$field'(_))->
    findall(F,'$field'(F),Fs)
  ; Fs=[].
  
  
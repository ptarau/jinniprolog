% REFLECTION API

/*
new_java_class(Name,Cls): 
  given a Name (Prolog constant), returns a handle to a Java class Cls 
  note that primitive types like int, double etc. also map to classes as given by their corresponding
  <ObjectType>.TYPE variables (of type Class).

new_java_object(Cls,Args,Obj): 
  returns Obj, an instance of class Cls by calling a constructor with Args. 
  If Args=none, a constructor with no arguments is called, if Args=args(A1,A2,...An) 
  then a matching constructor with arguments A1,A2...An is called, after appropriate conversions
   
invoke_java_method(Cls,Obj,MethName,Args,Answer): 
  Invokes on Obj a method named MethName of class or interface Cls with Args and unifies
  Answer with the value returned by the method (which can be one the special constant '$null'). 
  If Args=args(A1,A2,...An) then a matching method with arguments A1,A2...An is called, after
  appropriate conversions. 

get_java_field_handle(Obj,FieldName,FieldHandle): Given an object Obj and a FieldName
  it returns a FieldHandle (a handle to a java.lang.reflect.Field object) on which various
  get and set methods can then be invoked 
     
delete_java_class(Cls,YesNo): deletes Cls from the persistent object handle table. 
  YesNo=true indicates success.
  YesNo=fail indicates failure of the delete operation 

delete_java_object(Obj,YesNo): 
  deletes Obj from the persistent object handle table. 
  YesNo=fail indicates failure of the delete operation. 
*/

% DERIVED OPERATIONS

/*
  add new object using convenient <className>(A1,...An) syntax
  also supports '$object(Handle) format
*/

new_java_object(Class,Object):-
  handle2object(_,Class),
  !,
  new_java_object(Class,void,Object).
new_java_object(ClassName,Object):-
  atom(ClassName),
  !,
  new_java_class(ClassName,Class),
  new_java_object(Class,void,Object).
new_java_object(ClassAndArgs,Object):-
  functor(ClassAndArgs,ClassName,_),
  new_java_class(ClassName,Class),
  new_java_object(Class,ClassAndArgs,Object).

delete_java_objects(Os):-sort(Os,Ss),map(delete_java_object,Ss).

invoke_java_method(ObjectOrClass,MethodName,Args,Result):-
  invoke_java_method(guess_the_class,ObjectOrClass,MethodName,Args,Result).

/*
   invoke static or dynamic Java method using
   more natural <methodName>(A1,...An) syntax
*/
invoke_java_method(ObjectOrClass,MethodName,Result):-
  atom(MethodName),!,
  invoke_java_method(ObjectOrClass,MethodName,void,Result).
invoke_java_method(ObjectOrClass,MethodAndArgs,Result):-
  functor(MethodAndArgs,MethodName,_),
  invoke_java_method(ObjectOrClass,MethodName,MethodAndArgs,Result).

call_java_class_method(ClassName,MethodAndArgs,Result):-
  new_java_class(ClassName,Class),
  invoke_java_method(Class,MethodAndArgs,Result).

/*
  creates a new object using a class_name+args template
  to which it applies a method_args template
  note that the object as such is lost
*/
call_java_instance_method(ClassNameAndArgs,MethodAndArgs,Result):-
  new_java_object(ClassNameAndArgs,Object),
  invoke_java_method(Object,MethodAndArgs,Result),
  delete_java_object(Object,_).
    
/*
  Get the content of a (public) Java field 
  after obtaining a handle to it
*/
get_java_field(Object,FieldName,Val):-
  get_java_field_handle(Object,FieldName,Handle),
  invoke_java_method(Handle,get(Object),Val).

/*
  gets a static filed of a named class
*/
get_java_class_field(ClassName,FieldName,Val):-
  new_java_class(ClassName,Class),
  get_java_field(Class,FieldName,Val).

/*
  Set the content of a (public) Java field 
  to a new value, after obtaining a handle to it
*/
set_java_field(Object,FieldName,NewVal):-
  get_java_field_handle(Object,FieldName,Handle),
  invoke_java_method(Handle,set(Object,NewVal),_).

/*
  sets a static field of a named class
*/
set_java_class_field(ClassName,FieldName,NewVal):-
  new_java_class(ClassName,Class),
  set_java_field(Class,FieldName,NewVal).


/* calls static Array class methods */
invoke_array_method(MethodAndArgs,R):-
   call_java_class_method('java.lang.reflect.Array',
     MethodAndArgs,R).

/*
 makes a new array to hold Dim elements from class ClassName
*/
new_array(ClassName,Dim,Array):-
  integer(Dim),
  !,
  new_ndim_array(ClassName,[Dim],Array).
new_array(ClassName,[Dim|Dims],Array):-  
  new_ndim_array(ClassName,[Dim|Dims],Array).

new_ndim_array(ClassOrName,Dims,Array):-
  atomic(ClassOrName),
  !,
  new_java_class(ClassOrName,Class),
  new_ndim_array_of_class(Class,Dims,Array).
new_ndim_array(Class,Dims,Array):-
  new_ndim_array_of_class(Class,Dims,Array).
  
new_ndim_array_of_class(Class,Dims,Array):-
  new_java_object('prolog.logic.IntStack',Stack),
  foreach(
    member(Dim,Dims),
    invoke_java_method(Stack,push(Dim),_)
  ),
  invoke_java_method(Stack,toArray,Ints),
  invoke_array_method(newInstance(Class,Ints),R),
  delete_java_class(Class),
  delete_java_object(Stack),
  delete_java_object(Ints),
  R=Array.

/* moved to Java now part of new_java-class
new_java_primitive_class(Name,Class):-
  primitive_type(Name,PName),
  get_java_class_field(PName,'TYPE',Class).

primitive_type(boolean,'Boolean.TYPE').
primitive_type(char,'Character.TYPE').
primitive_type(byte,'Byte.TYPE').
primitive_type(short,'Short.TYPE').
primitive_type(int,'Integer.TYPE').
primitive_type(long,'Long.TYPE').
primitive_type(float,'Float.TYPE').
primitive_type(double,'Double.TYPE').
primitive_type(void,'Void.TYPE').
*/

/*
sets element I of array A
*/
array_set(A,I,E):-invoke_array_method(set(A,I,E),_).

/*
gets element I of array A
*/
array_get(A,I,R):-invoke_array_method(get(A,I),R).

/* gets alement of an N-dim array - index given as a list */
array_nget(A,[I|Is],R):-array_nget(Is,I,A,R).

array_nget([],I,A,R):-array_get(A,I,R).
array_nget([J|Is],I,A,R):-array_get(A,I,B),array_nget(Is,J,B,R).

/* sets alement of an N-dim array - index given as a list */
array_nset(A,[I|Is],R):-array_nset(Is,I,A,R).

array_nset([],I,A,V):-array_set(A,I,V).
array_nset([J|Is],I,A,R):-array_get(A,I,B),array_nset(Is,J,B,R).

/*
returns the number of elements of an array
*/
array_size(A,L):-invoke_array_method(getLength(A),L).

/*
converts a list to an array
*/

list_to_array(Xs,A):-list_to_array('java.lang.Object',Xs,A).

list_to_array(ClassOrName,Xs,A):-
  length(Xs,Dim),
  new_array(ClassOrName,Dim,A),
  foreach(nth_member0(X,Xs,I),array_set(A,I,X)).

/*
  converts a list of lists to an array
*/
nlist_to_array(Xsx,A):-nlist_to_array('java.lang.Object',Xsx,A).

% limited to 2-dim arrays, for now
nlist_to_array(ClassOrName,Xsx,A):-
  length(Xsx,D0),
  get_dim(Xsx,D1),
  new_array(ClassOrName,[D0,D1],A),
  ( nth_member0(Xs,Xsx,I),
      nth_member0(X,Xs,J),
        array_nset(A,[I,J],X),
    fail
  ; true
  ).
  
get_dim(Xss,Dim):-map(length,Xss,Ls),sort(Ls,Ns),(Ns=[SameD]->Dim=SameD;errmes(get_dim_error,have_different_length(Ls))).

/*
converts an array to a list
*/

array_to_list(A,Is):-array_size(A,Max),array_to_list(0,Max,A,Is).

array_to_list(Max,Max,_,[]).
array_to_list(I,Max,A,[X|Xs]):-I<Max,I1 is I+1,array_get(A,I,X),array_to_list(I1,Max,A,Xs).

array_to_nlist(A,Xsx):-array_to_nlist(A,2,Xsx).

/* converts an array to a list of lists of ... */
array_to_nlist(A,Dim,Xsx):-array_to_nlists(Dim,A,Xsx).

array_to_nlists(0,X,X).
array_to_nlists(N,A,Xss):-N>0,N1 is N-1,array_to_list(A,Xs),map(array_to_nlists(N1),Xs,Xss).

/* makes the use of booleans returned by Java easier */
is_true(B):-invoke_java_method(B,toString,T),T='true'.

is_false(B):-invoke_java_method(B,toString,F),F='false'.

bool2int(B,I):-invoke_java_method(B,toString,V),bval2int(V,I).

bval2int(false,0).
bval2int(true,1).

% BAD to_boolean(S,B):- call_java_class_method('java.lang.Boolean',getBoolean(S),B).
  
to_boolean(S,B):-new_java_object('java.lang.Boolean'(S),B).
  
/*
  Get the a handle to the class to which object O is an instance
*/  
get_java_class(O,Class):-
  invoke_java_method(O,getClass,Class).

delete_java_object(Object):-
  delete_java_object(Object,_YesNo).
  
delete_java_class(Class):-
  delete_java_class(Class,_YesNo).
  
  
% non-reflective simple builtins

jhandle(Name,Handle):-jhandle(Name,any,Handle).

jhandle(Name,Arg,Handle):-
  jcall(Name,Arg,S),
  S\=='$null',
  decode_jcall(S,Handle).
  
decode_jcall(S,Handle):-
  atom_codes(S,Cs),
  number_codes(Handle,Cs).

% accessing internals through reflection

getPrologName(Name):-
  call_java_class_method('prolog.kernel.Top',getPrologName,Name).

collect_call_to_string(Goal,Output):-call_java_class_method(
  'prolog.kernel.Top',collect_call(Goal),Output),
  reset_output.

collect_call(Goal,Cs):-
  collect_call_to_string(Goal,S),
  atom_codes(S,Cs).

set_string_input(String,InputStream):-
 invoke_java_method(String,getBytes,BS),
 new_java_object('java.io.ByteArrayInputStream',args(BS),InputStream),
 current_engine_object(E),
 invoke_java_method(E,setIntput(InputStream),_).

reset_input:-
 current_engine_object(E),
 invoke_java_method(E,resetInput,_).

set_string_output(OutputStream):-
 new_java_object('java.io.ByteArrayOutputStream',OutputStream),
 current_engine_object(E),
 invoke_java_method(E,setOutput(OutputStream),_).

reset_output:-
 current_engine_object(E),
 invoke_java_method(E,resetOutput,_).
 
collect_string_output(OutputStream,S):-
  object_to_string(OutputStream,S),
  reset_output.

% calls a goal from a serialized Prolog file

call_in_serialized(ClassName,Goal,OutputString):-
  new_string_output(OutputStream,Writer),
  call_java_class_method(
       'prolog.kernel.Top',
       new_cached_machine(ClassName,'$null',Writer),
     Handle),
  call_in_prolog(Handle,Goal),
  collect_string_output(OutputStream,OutputString).

new_string_output(OS,Writer):-
  new_java_object('java.io.ByteArrayOutputStream',OS),
  new_java_object('prolog.kernel.PrologWriter'(OS),Writer).

call_in_serialized(ClassName,Goal):-
  call_java_class_method(
       'prolog.kernel.Top',
       new_cached_machine(ClassName),
     Handle),
     call_in_prolog(Handle,Goal).

call_in_serialized(ClassName,Goal):-
  call_java_class_method(
       'prolog.kernel.Top',
       new_cached_machine(ClassName),
     Handle),
     call_in_prolog(Handle,Goal).
  
call_in_prolog(Handle,Goal):-     
  invoke_java_method(Handle,load_engine((Goal:-once(topcall(Goal)))),Ok),
  is_true(Ok),
  % handle2object(IntE,Handle),get(IntE,Answer),
  invoke_java_method(Handle,get_answer,Result),
  Result\=='$null',
  Result=Goal.

% calls a goal from a serialized Prolog file
call_in_new_serialized(ClassName,Goal):-
  namecat(ClassName,'.jc',File),
  call_java_class_method(
     'prolog.kernel.Top',
        new_machine(File),
        Handle),
   call_in_prolog(Handle,Goal).

% generic iterator interface - assuming it implements next() returning null at end

next_of(NextIterator,E):-next_of(NextIterator,next,E).

% more general - Advancer is any "next()" method
next_of(NextIterator,Advancer,E):-
  repeat,              
     ( invoke_java_method(NextIterator,Advancer,E),
       '$null'\==E->true
       ; !,delete_java_object(NextIterator),fail
     ).

/* calls restricted engine */

lcall0(Goal):-lcall0(Goal,the(Goal)).
lcall0(Goal,R):-nonvar(Goal),call_java_class_method('prolog.logic.LogicEngine','call'(Goal),R). 
lcall0(X,Goal,R):-lcall0((X:-Goal),R).

file(Path) :-
new_java_class('java.io.File',File),
new_java_object(File,args(Path),Dir),
dir <= Dir,
new_java_class('java.lang.reflect.Array',Array),
array <= Array.

list(Result) :-
dir => D,
invoke_java_method(D,list,Array),
list <= Array,
getItems(Array,Result).

name(Name) :-
list => L,
invoke_java_method(L,'getClass',Class),
invoke_java_method(Class,'getName',Name).

nameA(Name) :-
array => A,
invoke_java_method(A,'getClass',Class),
invoke_java_method(Class,'getName',Name).


length(Length) :-
list => L,
array => A,nameA(N),print('Name:'),println(N),
call_java_class_method(A,getLength(L),Length).

/*
?- renew(file,F), F:file('C:'),
F:list(R), invoke_java_method(R,'getClass',C),
invoke_java_method(C,'getName',N)

R = '$object'(1177)
F = '$instance'('$object'(1084),1)
N = '[Ljava.lang.String;'
C = '$object'(1172) ;
*/

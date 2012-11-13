add_cont(H,C,HC):-add_last_arg(H,C,HC).

to_number(Const,Num):-sread0(false,false,Const,N-[]),number(N),!,Num=N.
to_number(Const,Const).

to_string('$object'(O),S):-!,object_to_string('$object'(O),S).
to_string(Term,Const):-swrite(Term,Const).

term_codes(T,Cs):-nonvar(T),!,swrite(T,S),atom_codes(S,Cs).
term_codes(T,Cs):-atom_codes(S,Cs),sread(S,T-_).

symcat(A,B,R):-namecat(A,'_',B,R).

namecat(A,B,R):-namecat('',A,B,R).

make_cmd0(Lss,Cs):-make_cmd(Lss,C),atom_codes(C,Cs).
 
make_cmd([],'').
make_cmd([X|Xs],C2):-to_cmd(X,C0),make_cmd(Xs,C1),namecat(C0,C1,'',C2).

to_cmd(X,C):-atom(X),!,C=X.
to_cmd(X,C):-number(X),!,to_string(X,C).
to_cmd([X|Xs],C):-integer(X),!,atom_codes(C,[X|Xs]).
to_cmd(Bad,_):-throw(bad_data(make_cmd,Bad)).

insert_spaces([],[]).
insert_spaces([X|Xs],[' ',X|Ys]):-
  insert_spaces(Xs,Ys).

make_spaced_cmd([],'').
make_spaced_cmd([X|Xs],C):-insert_spaces(Xs,Ys),make_cmd([X|Ys],C).



to_lower_char(C,LC):- [A,Z,LA]="AZa",C>=A,C=<Z,!, LC is (LA-A)+C.
to_lower_char(C,C).

to_upper_char(LC,C):- [LA,LZ,A]="azA",LC>=LA,LC=<LZ,!, C is (A-LA)+LC.
to_upper_char(LC,LC).

to_upper_chars([],[]).
to_upper_chars([X|Xs],[Y|Ys]):-
  to_upper_char(X,Y),
  to_upper_chars(Xs,Ys).

to_lower_chars([],[]).
to_lower_chars([X|Xs],[Y|Ys]):-
  to_lower_char(X,Y),
  to_lower_chars(Xs,Ys).

object_to_string(O,S):-invoke_java_method(O,toString,S).

object_to_codes(O,Cs):-call_java_class_method('prolog.logic.TermConverter',object_to_codes(O),Cs).

object_to_codes(Len,Radix,O,Cs):-
  call_java_class_method('prolog.logic.TermConverter',object_to_codes(Len,Radix,O),Cs).

% array/ list conversion - to be made builtins!!!

% converts a prolog list to a Java array (keys: list_to_array, list2array)

list_array(Xs,A):-
  list_vector(Xs,S),
  invoke_java_method(S,toArray,A),
  delete_java_object(S).

list_vector(Xs,S):-
  new_java_object('prolog.logic.ObjectStack',S),
  add_all_members(Xs,S).

add_all_members(Xs,S):-
  member(X,Xs),
  invoke_java_method(S,push(X),_),
  fail.
add_all_members(_,_).

% converts an array to a Prolog list (keys: array2list, array_to_list)

array_list(Array,List):-findall(X,array_element_of(Array,X),List).

array_element_of(Array,X):-
  array_size(Array,L),
  Last is L-1,
  for(I,0,Last),
    array_get(Array,I,X).

queue_clone(Q,ClonedQ):-
  handle2object(Q,O),
  invoke_java_method(O,toClone,CO),
  handle2object(ClonedQ,CO).
  
set_encoding(Encoding):-
  call_java_class_method('prolog.kernel.Top',setEncoding(Encoding),_).

get_encoding(Encoding):-
  call_java_class_method('prolog.kernel.Top',getEncoding,Encoding).

force_encoding(String,FromFormat,ToFormat,Result):-
  call_java_class_method(
     'prolog.core.Transport',
     force_encoding(String,FromFormat,ToFormat),
     Result).

force_utf8(String,Result):-
  call_java_class_method(
     'prolog.core.Transport',
     force_utf8(String),
  Result).

external_string_tester:-
   [A,Z]="az",
   external_string_tester("hello",Hello),
   println(from_to(A,Z):Hello),
   statistics,
   for(I,A,Z),
     for(J,A,Z),
       for(K,A,Z),
         Codes=[I,J,K],
          external_string_tester(Codes,_SameCodes),
   fail.
external_string_tester:-
   nl,
   statistics.

/* calls java a method that processes a String that Prolog has
   never internalized into a symbol and returns
   a special functor '$STRING_S2CS'(String) that gets
   converted back to the same list of codes
*/
external_string_tester(Codes,SameCodes):-
   call_java_class_method(
       'prolog.logic.TermConverter',
       object_to_codes('$STRING_CS2S'(Codes)),
   SameCodes).

% Calendar / Date conversions
 
% to use for arithmetics on things like 2003/04/01 08:02:00 as in:

% ?- inc_date(date(2000,1,1,0,0,0),inc(0,13,0,0,1,1),R),format_date(R,S).

% args format: date(Y,M,D, H,N,S) and inc(Y,M,D, H,N,S) where N=minutes
inc_date(Date,Inc, R):-
  call_java_class_method('prolog.core.PDate',
    inc_date(Date,Inc),
  R).

% args format: date(Y,M,D, H,N,S)
format_date(Date, R):-
  call_java_class_method('prolog.core.PDate',
    format_date(Date),
  R).
  
% end
    
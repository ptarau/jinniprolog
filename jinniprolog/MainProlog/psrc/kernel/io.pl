/**************** -- INPUT --- ************************/


% fluent style readers

new_char_reader(Fname,Reader):- new_fluent(0,Fname,Reader).

new_term_string_reader(Fname,Reader):- new_fluent(1,Fname,Reader).

new_string_char_reader(String,Reader):- new_fluent(2,String,Reader).

new_string_term_string_reader(String,Reader):- new_fluent(3,String,Reader).

new_token_reader(Fname,Reader):- new_fluent(4,Fname,Reader).

new_string_token_reader(String,Reader):- new_fluent(5,String,Reader).

% compatible with BinProlog's read_line and read_words

read_words(Ws):-read_line(L),atom_codes(L,Cs),codes_words(Cs,Ws).

write_words(Ws):-nonvar(Ws),!,write_ws(Ws).
write_words(Ws):-errmes(list_expected,found(Ws)).

write_ws(Ws):-words2nat(Ws,Ns),member(N,Ns),name(N,Cs),member(C,Cs),put(C),fail.
write_ws(_).

words2nat(Ws,Ns):-words2nat(Wss,Ws,[]),appendN(Wss,Vs),once(append(Us,[S],Vs)),(' '=S->Ns=Us;Ns=Vs).

words2nat([])-->[].
words2nat([[W,C,' ']|Wss])-->[W,C],{left_collider(C)},!,words2nat(Wss).
words2nat([[L,C]|Wss])-->[L,C],{collider(C)},!,words2nat(Wss).
%words2nat([[Q,W,Q,' ']|Wss])-->[Q],{Q=('"')},[W],[Q],!,words2nat(Wss).
words2nat([[Q|Vs],[Q]|Wss])-->[Q],{Q=('"')},match_before(Q,Ws),!,{words2nat(Ws,Vs)},words2nat(Wss).
words2nat([[W,' ']|Wss])-->[W],words2nat(Wss).

left_collider(W):-member(W,[(','),(':'),(';'),('.'),('!'),('?')]).

collider(W):-member(W,[(''''),(-)]).

% reads line or fails

read_line(L):-readln(L),L\=='$null'.

% read from stdio - see also gui_readln
  
readln(LineString):-
  current_engine_object(O),
  invoke_java_method(O,readln,LineString).

flush:-
  current_engine_object(O),
  invoke_java_method(O,flush,_).

get_line_number(Line):-
  current_engine_object(O),
  invoke_java_method(O,getLineNumber,Line).

set_line_number(Line):-
  current_engine_object(O),
  invoke_java_method(O,setLineNumber(Line),_).
  
get_text_sink(Handle):-
  current_engine_object(O),
  invoke_java_method(O,getTextSink,Handle).


/*
% compiled code specific IO operations
% true=means that warnings will be produces on singleton vars

% scall(SClause,G):-sread_term(SClause,G),G.
*/

sread(T,CVs):-sread0(true,true,T,CVs).
  
sread_term(STerm,Term):-sread(STerm,Term-_).

sread_term(STerm,Term,Vs):-sread(STerm,Term-Vs).

sread_term(STerm,Term,Vars,Names):-
  sread_term(STerm,Term,NVs),
  split_varlist(NVs,Names,Vars).

split_varlist([],[],[]).
split_varlist([(N=V)|NVs],[N|Ns],[V|Vs]):-
  split_varlist(NVs,Ns,Vs).
 
% reads a clause
sread_clause(STerm,clause(Clause,Vars,Names)):-
  sread_term(STerm,Term,Vars,Names),
  to_clause(Term,Clause).

read_term(T,Vs):-sread_term(1,T,Vs).

% calls java or prolog parser, selectively

read(T):-check_full_parser,!,p_read(T).
read(T):-read_goal(T,_).

read_goal(Term):-read_goal(Term,_).
 
read_goal(Term,Vs):-
  read_goal(1,Term,Vs).
  
read_goal(STerm,Term,Vs):-
  qsread_goal(STerm,Term,Vs).
  
% reads a goal from a string quietly
qsread_goal(STerm,Term,NVs):-
  sread0(false,false,STerm,Term-NVs).

% reads a goal from a string  
sread_goal(STerm,Term,NVs):-
  sread0(false,true,STerm,Term-NVs).
  
string_clause_reader(S,R):-
  new_engine(C,sread_clauses(S,C),R).

sread_clauses(S,C):-sread_terms(S,T),to_clause(T,C).
  
sread_terms(S,C):-sread_terms(S,C,_).

sread_terms(S,T,Vs):-
  new_string_term_string_reader(S,R),
  good_element_of(R,X),
  sread(X,T-Vs).
 
% binary file input

get_code(X):-get0(X).
get_char(C):-get0(X),char_code(C,X).

get(R):-repeat,get0(X),(X>32;X<0),!,R=X.

see(File):-see_or_fail(File),!.
see(File):-user_error(unable_to_see,File).

exists_file(F):-
  seeing(G),
  ( see_or_fail(F)->seen,see(G)
  ; fail
  ).

% gloabal persistent properties

get_global_prop(K,V):-call_java_class_method('prolog.kernel.Top',getProp(K),V).

set_global_prop(K,V):-call_java_class_method('prolog.kernel.Top',setProp(K,V),_).

rm_global_prop(K):-call_java_class_method('prolog.kernel.Top',rmProp(K),_).

clear_global_props:-call_java_class_method('prolog.kernel.Top',clearProps,_).

save_global_props(File):-call_java_class_method('prolog.kernel.Top',saveProps(File),_).

load_global_props(File):-call_java_class_method('prolog.kernel.Top',loadProps(File),_).

save_global_props:-save_global_props('prolog.jp').

load_global_props:-load_global_props('prolog.jp').

set_verbosity(N):-
  call_java_class_method('prolog.kernel.Top',set_verbosity(N),_).

get_verbosity(N):-
  call_java_class_method('prolog.kernel.Top',get_verbosity,N).

set_quickfail(N):-
  call_java_class_method('prolog.kernel.Top',set_quickfail(N),_).

get_quickfail(N):-
  call_java_class_method('prolog.kernel.Top',get_quickfail,N).
    
get_prolog_home(S):-call_java_class_method('prolog.kernel.JavaIO',getPrologHome,S).

set_prolog_home(S):-call_java_class_method('prolog.kernel.JavaIO',setPrologHome(S),_).
 
get_path(Ds):-
  findall(D,(path_element(F),atom_codes(F,D)),Ds).

add_to_path(F):-jcall(add_to_path,F,_).

push_to_path(F):-jcall(push_to_path,F,_).
  
del_from_path(F):-jcall(del_from_path,F,_).

clear_path:-jcall(clear_path,any,_).
  
path_element(E):-
  jhandle(user_path,Q),
  queue_size(Q,N),N1 is N-1,
  for(I,0,N1),to_string(I,S),
  jcall(path_element,S,E).
  
find_file(FileDescr,NewFile):-
  to_filename(FileDescr,File),
  get_path(Ds),
  get_suffix_list(SuffixesCss),
  find_file(File,
    Ds,
    SuffixesCss,
    NewFile
  ).

to_filename(A,N):-atomic(A),!,N=A.
to_filename('/'(A,Bs),N):-to_filename(Bs,M),namecat(A,'/',M,N).

get_suffix_list(Css):-
  call_ifdef(suffix_list(Css0),default_suffix_list(Css0)),
  Css=Css0.

default_suffix_list(
  [".pl","",".pro"] %,".txt"],
).
     
find_file(File,Prefs,Sufs,NewFile):-
	seeing(CF),
	find_file1(CF,File,Prefs,Sufs,NewFile).

find_file1(CF,File,Prefs,Sufs,NewFile):-
	see_a_file(Prefs,File,Sufs,NewFile),
	!,
	see(CF).
find_file1(CF,File,Prefs,Sufs,_):-
	see(CF),
  findall(NP,(member(P,Prefs),atom_codes(NP,P)),NPrefs),
  findall(SP,(member(S,Sufs),atom_codes(SP,S)),NSufs),
  get_prolog_home(Home),
  Errmes=tried(home(Home),prefixes(NPrefs),sufixes(NSufs)),
	errmes(
	  file_or_url_not_found(File),
	  Errmes
	).

see_a_file(_,File,_,NewFile):-exists_file(File),!,NewFile=File.
see_a_file(Prefs,File,Sufs,NewFile):-
  member(Suf,Sufs),
  member(Pref,Prefs),	
  atom_codes(File,L),
  det_append(L,Suf,R),
  det_append(Pref,R,Fname),
  atom_codes(NewFile,Fname),
  see_or_fail(NewFile),
  seen.

% Prolog file input

pred_of(F,FN):-
  find_file(F,File),
  file_clause_reader(File,ClauseSource),
  preds_of_reader(ClauseSource,Preds),
  member(FN,Preds).
  
preds_of_reader(ClauseSource,Preds):-
  findall(FN,
     multiple_pred_element_of(ClauseSource,FN),
     WithDups
  ),
  sort(WithDups,Preds).
    
multiple_pred_element_of(ClauseReader,F/N):-
  good_element_of(ClauseReader,(H:-_)),
  functor(H,F,N).

clause_of(F,C):-
  term_of(F,T),
  to_clause(T,C).

clause_of(F,C,Vs):-
  term_of(F,T,Vs),
  to_clause(T,C).
 
/*   
bad_term_of(F,C):-
  term_string_of(F,S),
  sread0(true,true,S,C-_),
  %!!! safe only if known that no existing constant is read from the file
  delete_java_object(S).
*/


term_of(F,C):-check_full_parser,!,p_term_of(F,C).
term_of(F,C):-jterm_of(F,C).

jterm_of(F,C):-term_of(F,C,_).
  
term_of(F,C,Vs):-
  term_string_of(F,S),
  sread(S,C-Vs).

term_string_of(F0,S):-
  find_file(F0,F),
  new_term_string_reader(F,R),
  good_element_of(R,S).

url2tokens(URL,Cs):-findall(C,token_of(URL,C),Cs).

token_of(F0,T):-
  find_file(F0,F),
  new_token_reader(F,R),
  good_element_of(R,T).

token_of_string(S,T):-
  new_string_token_reader(S,R),
  good_element_of(R,T).

codes_words(Cs,Ws):-var(Cs),!,
  findall(C,words_code(Ws,C),Cs).
codes_words(Cs,Ws):-
  atom_codes(S,Cs),
  findall(T,token_of_string(S,T),Ws).



words_code(Ws,C):-words2nat(Ws,Ns),member(N,Ns),name(N,Xs),member(C,Xs).

file2chars(FileOrURL,Cs):-url2codes(FileOrURL,Cs).

url2codes(URL,Cs):-findall(C,char_of(URL,C),Cs).

url2string(URL,S):-url2codes(URL,Cs),atom_codes(S,Cs).

file2string(F,S):-url2string(F,S).

% faster - return directly a new String - that should be deleted later
url2new_codes(URL,Cs):-url2string(URL,S),atom_codes(S,Cs). % adds garbage symbol

url2new_string(URL,S):-find_file(URL,F),call_java_class_method('prolog.kernel.JavaIO',url2string(F),S).

reader2new_string(R,S):-call_java_class_method('prolog.kernel.JavaIO',reader2string(R),S).

% backtracking iterators

char_of(F,C):-
  find_file(F,URL),
  %F=URL,
  new_char_reader(URL,Reader),
  element_of(Reader,C).

sentence_of(F,Ws):- 
  Ends=".!?",sentence_of(F,Ends,Xs),
  codes_words(Xs,Ws).

line_of(F,Cs):-sentence_of(F,[10],Cs).
     
sentence_of(URL,Ends,Xs):-
  find_file(URL,F),
  new_char_reader(F,Reader),
  pick_code_of(Reader,Ends,Xs).

pick_code_of(F,Ends,Ys):-
  getx(F,X),
  collectx(Ends,X,F,Xs,More),
  select_code_from(F,Ends,Xs,Ys,More).

select_code_from(_,_,As,As,_).
select_code_from(I,Ends,_,Xs,yes):-pick_code_of(I,Ends,Xs).

collectx(_,no,_,".",no):-!.
collectx(Ends,the(End),_,[End],yes):-member(End,Ends),!.
collectx(Ends,the(X),F,[X|Xs],More):-
   getx(F,NewX),
   collectx(Ends,NewX,F,Xs,More).
    
getx(F,X):-get(F,X0),hide_nls(X0,X).

% hide_nls(the(10),the(32)):-!.
hide_nls(the(13),the(32)):-!.
hide_nls(X,X).

/************* OUTPUT ************************/
 
put_code(X):-put(X).
put_char(C):-char_code(C,X),put(X). 


write(X):-atomic(X),!,name(X,Cs),write_codes(Cs).
write(X):-cwrite(X).

% write(X):-cwrite(X).

print(X):-write(X).
writeq(X):-cwrite(X).

write_unquoted(T):-
  term_codes(T,Cs),
  trim_quotes(Cs,Us),
  write_codes(Us).
  
%  flush.

% for BinProlog compatibility
write_chars(Cs):-write_codes(Cs).

write_codes(Cs):-
  member(C,Cs),
  put(C),
  fail.
write_codes(_).  

trim_quotes(Cs,Us):-
  findall(C,qtrimmed_code(Cs,C),Us).

qtrimmed_code(Cs,C):-
  "'"=[Q],
  member(C,Cs),
  C=\=Q.

/*
qtrimmed_code(Cs,C):-
  nonvar(Cs),
  length(Cs,N),
  [Q]="'",
  nth_member(C,Cs,I),
  ( I=:=1,C=:=Q->fail
  ; I=:=N,C=:=Q->fail
  ; true
  ).
*/

set_format_precision(N):-
  current_engine_object(Machine),
  invoke_java_method(Machine,setFormatPrecision(N),_).
  
cnl:-nl. % atom_codes(NL,[10]),cwrite(NL).

println(X):-cwrite(X),cnl.

% writeln(X):-sleep_ms(5),println(X),sleep_ms(5).

writeln(X):-println(X).

ttynl:-telling(F),tell(user),nl,tell(F).

ttyput(X):-telling(F),tell(user),put(X),tell(F).

ttyprint(X):-telling(F),tell(user),println(X),tell(F).

tell(File):-tell_or_fail(File),!.
tell(File):-user_error(unable_to_tell,File).

tab(N):-for(_,1,N),put(32),fail.
tab(_).

debugmes(M):-val(debug,yes),ttyprint(M).
debugmes(_).

string2file(S,F):-
  telling(TF),
  tell(F),
  atom_codes(S,Xs),
  write_codes(Xs),
  told,
  tell(TF).

getErrmes(Val):-
  get_java_class_field('prolog.kernel.Interact','showErrors',B),
  invoke_java_method(B,toString,Val).
setErrmes(Val):-
  to_boolean(Val,B),
  set_java_class_field('prolog.kernel.Interact','showErrors',B).
  


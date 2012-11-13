www_root('..').

data_dir('../data').
cgi_dir('../cgi').
ssi_dir('../ssi').

to_data_dir:-data_dir(D),cd(D).
to_ssi_dir:-ssi_dir(D),cd(D).
to_cgi_dir:-cgi_dir(D),cd(D).

/*
 Internet Programming Toolkit
 Copyright (C) Paul Tarau 1999

 Parsing/String processing tools
*/

split_path_file(DsFs,NewDs,Fs):-
  [Slash,Dot]="/.",
  reverse(DsFs,Rs),
  !,
  ( match_before([Slash],RFs0,_,Rs,End1),member(Dot,RFs0)->true
  ; RFs0=""
  ),
  RDs0=End1,
  !,
  ( % swap dir and file if dir looks like *.*
    RFs0="",member(Dot,RDs0)->RDs="",RFs=RDs0
  ; RDs=RDs0,RFs=RFs0
  ),
  (RDs=[Slash,Slash|As]->NewRDs=[Slash|As]
  ; \+(RDs=[Slash|_]), [QM]="?", \+(member(QM,RDs)) ->NewRDs=[Slash|RDs]
  ; NewRDs=RDs
  ),
  reverse(NewRDs,Ds),
  (Ds=[Slash,Slash|Xs]->Ds=[Slash|Xs]
  ; \+(Ds=[Slash|_])->NewDs=[Slash|Ds]
  ; NewDs=Ds
  ),
  reverse(RFs,Fs).

  
% recognizes obvious text/html/VRML etc. files which may contains links
is_text_file(''):-!. % might be dir or cgi-bin
is_text_file(File):-
  name(File,Fs),
  has_text_file_sufix(Fs,_).

% detects files likely to be text files containing http:// links
has_text_file_sufix(Fs,Suf):-
  reverse(Fs,Rs),
  % known as text files
  member(Suf,
      [".html",".htm",".shtml",".asp",".txt",".wrl",".pro",".pl",".java",".c",
       ".xml","xsl",
       ".HTML",".HTM",".SHTML",".ASP",".TXT",".WRL",".PRO",".PL",".JAVA",".C",
       ".XML","XSL"
      ]),
  reverse(Suf,RSuf),
  append(RSuf,_,Rs),
  !.

has_known_file_sufix(Fs,Suf):-has_text_file_sufix(Fs,Suf),!.
has_known_file_sufix(Fs,Suf):-
  reverse(Fs,Rs),
  % known as text files
  member(Suf,
      [".jpg",".gif"
      ,".JPG",".jpg"
      ]),
  reverse(Suf,RSuf),
  append(RSuf,_,Rs),
  !.
  
% conversion tools

% replaces some chars with their hex-escaped %XY versions

hex_escape(Cs,Hs):-hex_escape(Cs,Hs,[]).

hex_escape([],Cs,Cs).
hex_escape([C|Cs],HHs,Hs):-hex_convert(C,HHs,Xs),hex_escape(Cs,Xs,Hs).

hex_convert(C,Cs):-hex_convert(C,Cs,[]).

hex_convert(C,Cs,Bs):-ok_as_is(C),!,Cs=[C|Bs].
hex_convert(C,Cs,Bs):-special_to_hex(C,Cs,Bs).

ok_as_is(0'%).
ok_as_is(0'/).
ok_as_is(0'|).
ok_as_is(0'.).
ok_as_is(0'-).
ok_as_is(0'+).
ok_as_is(0'?).
ok_as_is(0'&).
ok_as_is(C):-is_an(C).

special_to_hex(Spec,Cs):-special_to_hex(Spec,Cs,[]).

special_to_hex(Spec,[37,A,B|Cs],Cs):-
  DA is Spec // 16,
  DB is Spec mod 16,
  to_hex_char(DA,A),
  to_hex_char(DB,B).

to_hex_char(DA,A):-DA>=10,!,A is 0'A+DA-10.
to_hex_char(DA,A):-A is 0'0+DA.

debug_print(Css):-
    quiet(Q),Q<2,member(Cs,Css),write_chars(Cs),nl,
  fail.
debug_print(_).


% reads a file line by line, backtracks until end, then fails

file2line(Path,File0,Cs):-
  namecat(Path,'',File0,File),
  exists_file(File),
  seeing(F),see(File),
    repeat,
    (read_chars(Cs)->true
    ; !,seen,see(F),fail
    ).

/* PORTING to 9x */

socket_try(_,Goal):-Goal,!.
socket_try(Socket,Goal):-
  close_socket(Socket),
  errmes(server_error_failing_goal,Goal).

/*
reverse(List, Reversed) :-
	reverse(List, [], Reversed).

reverse([], Reversed, Reversed).
reverse([Head|Tail], SoFar, Reversed) :-
	reverse(Tail, [Head|SoFar], Reversed).
*/

/* end of PORTING */

% applications should implement post_method_handler/3

default_post_method_handler(Alist,Template,Template):-
   show_input_data(Template,Alist).

% post method ssi wrapper - should succeed ONCE and NOT fail !!!
   
post_method_wrapper(PostQuery,OutputTemplate,Output):-
  % println(jinni(post_method_handler_got(PostQuery))),
  atom_codes(PostQuery,AlistCs),
  split_post(AlistCs,Alist),
  call_ifdef(
    post_method_handler(Alist,OutputTemplate,Output),
    default_post_method_handler(Alist,OutputTemplate,Output)
  ),
  !.
post_method_wrapper(Q,O,O):-
  println(post_query_failed(Q)).

show_input_data(Template,XVs):-
  write_codes("TEMPLATE: "),write(Template),nl,
  write_codes("ALIST:"),nl,
  member(X=Vs,XVs),
  write_words([X]),
  write_codes("="),
  write_codes(Vs),
  nl,
  fail.
show_input_data(_,_).  
  
% POST method alist parser

split_post(Cs,Ws):-
  post(Ws,Cs,End),
  !,
  End=[].
   
post([W|Ws])-->assoc(W),star(dassoc,Ws).

assoc(Key=NewYs) --> 
  plus(nodelim,Xs),delim("="),star(nodelim,Ys),
  { 
    parse_input(Xs,NewXs),
    parse_input(Ys,NewYs),
    name(Key,NewXs)
  }.

dassoc(Xs) --> delim("&"),assoc(Xs).

delim([X]) --> [X].

nodelim(X) --> [X], {\+(member(X,"&="))}.

parse_input([],[]).
parse_input([X|Xs],YYs):-
  ( [X]="%",Xs=[A,B|NewXs],compute_code(A,B,Y)-> 
    (Y=:=13->YYs=Ys;YYs=[Y|Ys])
  ; [X]="+"->NewXs=Xs,YYs=[32|Ys]
  ; NewXs=Xs,YYs=[X|Ys]
  ),
  parse_input(NewXs,Ys).

% hex to decimal

compute_code(A,B,Y):-
  to_digit(A,DA),to_digit(B,DB),
  Y is 16*DA+DB.
	
%digits
to_digit(48,0).
to_digit(49,1).
to_digit(50,2).
to_digit(51,3).
to_digit(52,4).
to_digit(53,5).
to_digit(54,6).
to_digit(55,7).
to_digit(56,8).
to_digit(57,9).

% A..F
to_digit(65,10).
to_digit(66,11).
to_digit(67,12).
to_digit(68,13).
to_digit(69,14).
to_digit(70,15).

% a..f
to_digit(97,10).
to_digit(98,11).
to_digit(99,12).
to_digit(100,13).
to_digit(101,14).
to_digit(102,15).

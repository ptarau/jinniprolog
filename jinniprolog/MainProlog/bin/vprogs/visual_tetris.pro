%  Hor Vert Size  - start at 0 - increase left-to-right and downward
%  TODO: memory of discarded GUI objects needs to be recovered - change scr_send?

test:-
  init_board,
  sleep(2),
  val(tetris,message,M),
  set_label(M,'TESTING.....'),
  new_color(1,0,1,Color),
  draw_color(1,1,Color),
  sleep(2),
  scr_stat(99),
  sleep(5),
  val(tetris,frame,F),
  destroy(F).
 
small_test:-
  is_prolog(jinni_compiled),
  new_frame(main_frame,border,Frame),
  new_color(1,0,1,Pink),
  set_bg(Frame,Pink),
  show(Frame).

% visualisation

init_board:-
  Logo='Tetris Player - (C) Paul Tarau 1989-2004',
  dims(HSize,VSize),
  clean_up_actions,
  new_color(0,1,0,Empty),
  new_color(1,0,0,Full),
  let(color,empty,Empty),
  let(color,full,Full),
  Q is 3/4,
  new_color(Q,Q,Q,ButtonColor),
  new_color(0,0,1,Blue),
  new_color(1,0,0,EndColor),
  new_frame(Logo,border,Frame),
    set_direction(Frame,'Center'),
    new_panel(Frame,grid(HSize,VSize),Game),
      set_bg(Game,Blue),
    set_direction(Frame,'South'),
    new_panel(Frame,grid(2,1),CtrlPan),
      new_panel(CtrlPan,border,DispPan),
        set_direction(CtrlPan,'Center'),
        new_label(DispPan,'Score=      ',Score),
        set_direction(DispPan,'East'),
        new_button(DispPan,end,scr_end,End),
        set_bg(End,EndColor),
      new_label(CtrlPan,Logo,Mes),
        set_bg(Mes,ButtonColor),
    set_direction(Frame,'East'),    
    new_panel(Frame,border,Buttons),
        set_bg(Buttons,ButtonColor),
      set_direction(Frame,'Center'),    
      %new_panel(Buttons,border,Center),
        new_action(Buttons,'Center',Blue,turn), 
        new_action(Buttons,'North',ButtonColor,up),
        new_action(Buttons,'South',ButtonColor,down),
        new_action(Buttons,'East',ButtonColor,right),
        new_action(Buttons,'West',ButtonColor,left),
  init_virtual(Game,Empty),
  % resize(Frame,V,H),
  let(tetris,frame,Frame),
  let(tetris,message,Mes),
  let(tetris,score,Score),
  show(Frame).

init_virtual(Game,Empty):-
  dims(HSize,VSize),
  H is HSize-1,
  V is VSize-1,
  for(I,0,H),
    for(J,0,V),
      new_label(Game,'',Panel),
      set_bg(Panel,Empty),
      let(I,J,Panel),
  fail.
init_virtual(_,_).
  
draw_color(I,J,Color):-
  val(I,J,Panel),
  call(set_bg(Panel,Color)).

char2color(' ',Color):-val(color,empty,Color).
char2color('*',Color):-val(color,full,Color).

/* button actions */

new_action(BorderPanel,Dir,Color,A):-
  set_direction(BorderPanel,Dir),
  new_button(BorderPanel,A,put_action(A),B),
  set_bg(B,Color).

put_action(A):-asserta(action(A)).

get_action(A):-retract(action(A)),!.
    
clean_up_actions:-retractall(action(_)).
      
action_dir(left,2).
action_dir(up,3).
action_dir(turn,-1).
action_dir(down,1).
action_dir(right,0).

/* 
   (local) i/o actions - used to update the display
   they can be called remotely when in client/server 
   or background thread mode
*/
   
% reading a direction: default falling direction

scr_dir(D):-get_action(A),!,
  action_dir(A,D),
  let(tetris,action,A). 
scr_dir(1):-
  ( val(tetris,action,A)->
    ( A==down->T=200
    ; T is 1000
    )
  ; T is 200
  ),
  sleep_ms(T).

dtime(DT):-statistics(runtime,[_,DT]).

scr_init(N):-
  init_board,
  N=0,scr_score(N),
  !.

stopper:-val(tetris,stopped,yes),rm(tetris,stopped),do_end_game.
  
scr_end:-def(tetris,stopped,yes),bg(run_final_cleaner).

run_final_cleaner:-
  sleep(10),
  do_end_game,
  fail.
  
  
do_end_game:-
  ( val(tetris,frame,F)->destroy(F)
  ; true
  ),
  crm(tetris,stopped),
  clean_up_actions,
  crm(tetris,frame),
  crm(tetris,score),
  crm(tetris,message),
  crm(tetris,action).

crm(K1,K2):-val(K1,K2,_),!,rm(K1,K2).
crm(_,_).

scr_send(p(L,C),Code):-
  char_code(Char,Code),
  char2color(Char,Color),
  draw_color(L,C,Color).

scr_score(Score):-
  val(tetris,score,ScoreBoard),
  make_cmd(['Score: ',Score],Cmd),
  set_label(ScoreBoard,Cmd).

scr_stat(Val):-
  statistics(global_stack,[HStat,_]),
  statistics(symbols,[SStat,_]),
  statistics(runtime,[_,T]),
  make_cmd(['Energy=',Val,',Heap=',HStat,',Syms=',SStat,',Time=',T],Cmd),
  val(tetris,message,M),
  set_label(M,Cmd).

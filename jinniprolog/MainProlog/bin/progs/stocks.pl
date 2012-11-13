% Author: Paul Tarau

go:-go(20,1).

go(Fuel,SleepTime):-market(Fuel,SleepTime,0.09,0.01).

% for Jinni2k only
synchronize(G):-G.
% interface

% stock(Name,Price),
% has(Agent,Stock,Qty),
% capital(Agent,Value).

market(Seconds,SleepTime,Variation,Direction):-
  %Seconds=upper limit on total execution time
  %SleepTime= put 1..5 seconds - to slow it down the ticker
  %Variation=0.09, % max up or down movement at each tick
  %Direction=0.001, % growing slowly ...
  bg(new_ticker(Variation, Direction, SleepTime)), % starts bg ticker
  make_market, % puts a number of stocks on the market
  start_agents, % runs the trading agents
  sleep(Seconds), % let the market run for a few Seconds
  synchronize(stop_market). % exits the program

stop_market:-
  nl,println('Market closed'),nl,show,
  stop.

make_market:-
  put_stock(lcos,32),
  put_stock(xcit,40.5),
  put_stock(dell,69),
  put_stock(yhoo,120),
  put_stock(inkt,82).


start_agents:-
  new_agent(jamesBond,25000),
  new_agent(mataHari,25000),
  run_agent(jamesBond,[
   buy_limit(lcos,31,300),
   buy_limit(xcit,40,100),
   buy_limit(dell,69,60),
   buy_limit(yhoo,121,50),
   buy_limit(inkt,81.75,80),
   sell_limit(lcos,33,300),
   sell_limit(xcit,41,100),
   sell_limit(dell,70,60),
   sell_limit(yhoo,130,50),
   sell_limit(inkt,83,80)
  ]),
  run_agent(mataHari,[
   buy_market(lcos,300),
   buy_market(xcit,100),
   buy_market(dell,60),
   buy_stop(yhoo,125,50),
   buy_stop(inkt,85,30),
   sell_stop(lcos,25,300),
   sell_stop(xcit,30,100),
   sell_limit(dell,70,60),
   sell_limit(yhoo,130,50),
   sell_limit(inkt,83,80)
  ]). 
  

new_agent(Name,Capital):-nonvar(Name),number(Capital),
  out(capital(Name,Capital)).

run_agent(Name,Todo):-foreach(member(Op,Todo),bg(call(Op,Name))).

% Delta = scale of +/- change and 
% N = sleeping interval in sec
new_ticker(Variation,Direction,N):-
  repeat,
    get_stock(Name,Base),
    is(Scale,'*'(Base,Variation)),
    scale_random_val(Scale,Base,Val1),
    is(Half,'/'(Scale,2)),
    is(Val2,'-'(Val1,Half)),
    is(Val,'*'(Val2,'+'(1,Direction))),
    max_of(0.1,Val,NewVal),
    put_stock(Name,NewVal),
    synchronize(show_ticker(Name,NewVal)),
    sleep(N),
  fail.

show_ticker(Name,NewVal):-
 println(ticker(stock(Name),price(NewVal))),nl.

max_of(A,B,M):-once(max0(A,B,M)).

max0(A,B,M):- '<'(A,B),'='(M,B).
max0(A,_,A).
 
scale_random_val(Scale,Base,Random):-
 is_prolog(Prolog),
 once(scale_random_val(Prolog,Scale,Base,Random)).
 
scale_random_val(binprolog,Scale,Base,Random):-
  random(R),is(X,'/'(mod(R,1000),1000)),
  is(Random,'+'('*'(Scale,X),Base)).
scale_random_val(jinni_compiled,Scale,Base,Random):-
  random(R),is(X,'/'(mod(R,1000),1000.0)),
  is(Random,'+'('*'(Scale,X),Base)).
scale_random_val(jinni_interpreted,Scale,Base,Random):-
  compute('random',Scale,Base,Random).
scale_random_val(jinni,Scale,Base,Random):-
  compute('random',Scale,Base,Random).

put_stock(Name,Val):-
  notify_about(stock(Name,Val)).

get_stock(Name,Val):-get_stock(Name,true,Val).

get_stock(Name,Cond,Val):-
  wait_for(stock(Name,Val),Cond).

watch_stock(Name,Price):-
  get_stock(Name,Price),
  put_stock(Name,Price).

buy_market(Name,Qty,Agent):-
  watch_stock(Name,Price),
  buy(Agent,Name,Price,Qty).

buy_limit(Name,Limit,Qty,Agent):-
  get_stock(Name,'=<'(Price,Limit),Price),
  put_stock(Name,Price),
  buy(Agent,Name,Price,Qty).

buy_stop(Name,Limit,Qty,Agent):-
  get_stock(Name,'>='(Price,Limit),Price),
  put_stock(Name,Price),
  buy(Agent,Name,Price,Qty).

sell_market(Name,Qty,Agent):-
  watch_stock(Name,Price),
  sell(Agent,Name,Price,Qty).

sell_limit(Name,Limit,Qty,Agent):-
  get_stock(Name,'>='(Price,Limit),Price),
  put_stock(Name,Price),
  sell(Agent,Name,Price,Qty).
   
sell_stop(Name,Limit,Qty,Agent):-
  get_stock(Name,'=<'(Price,Limit),Price),
  put_stock(Name,Price),
  sell(Agent,Name,Price,Qty).

buy(Me,Stock,Price,Qty):-
    in(capital(Me,MyCapital)),    % operations
    is(Cost,'*'(Price,Qty)),
    is(MyNewCapital,'-'(MyCapital,Cost)),
    out(capital(Me,MyNewCapital)),
    if(cin(has(Me,Stock,OldQty)),
      is(NewQty,'+'(OldQty,Qty)),
      eq(NewQty,Qty)
    ),
    out(has(Me,Stock,NewQty)),
	show(Me,buy(Stock,Price,Qty)).

sell(Me,Stock,Price,Qty):-
    in(capital(Me,MyCapital)),    % operations
    is(Cost,'*'(Price,Qty)),
    is(MyNewCapital,'+'(MyCapital,Cost)),
    out(capital(Me,MyNewCapital)),
    if(cin(has(Me,Stock,OldQty)),
      is(NewQty,'-'(OldQty,Qty)),
      is(NewQty,'-'(0,Qty)) % sell short
    ),
    out(has(Me,Stock,NewQty)),
	show(Me,sell(Stock,Price,Qty)).

show:-synchronize(show0).
show(Name,Action):-synchronize(show0(Name,Action)).

show0(Name,Action):-
 println(agent(Name,Action)),
 eq(Capital,capital(Name,Cash)),
 all(Capital,Cs),
 all_for(stock(_,_),StockPrices),
 member(Capital,Cs),
   all(has(Name,_,_),Holdings), 
   foreach(member(X,Holdings),println(X)),
   compute_assets(Holdings,StockPrices,Val),
   is(Total,'+'(Cash,Val)),
   println(capital(Cash)),
   println(stocks(Val)),
   println(total(Total)),
 nl.

compute_assets(List,StockPrices,Val):-
   compute_assets(List,StockPrices,0,Val).

compute_assets([],_,SoFar,SoFar).
compute_assets([has(_,Stock,Qty)|Hs],StockPrices,SoFar,Val):-
  once(member(stock(Stock,Price),StockPrices)),
  is(Temp,'+'(SoFar,'*'(Price,Qty))),
  compute_assets(Hs,StockPrices,Temp,Val).

show0:-all_for(stock(_,_),Xs),member(X,Xs),println(current(X)),fail.
show0:-nl,all(Name,capital(Name,_),Ns),member(N,Ns),show0(N,showing),fail.
show0.

clean(X):-all(X,Xs),foreach(member(X,Xs),cin(X)).

clean:-
  clean(has(_,_,_)),
  clean(capital(_,_)).

% tests

ttest:- 
  make_market, % puts a number of stocks on the market
  show,
  new_ticker(0.09,0.001,1), % starts bg ticker for 20 secs
  show.



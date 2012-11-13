% w1

b:-broker:start.

% w2

s:-new_rserver(boo,S),rserver_start(S),rserver_do(S,a<=99),a=>X,println(X),
   rserver_get_port(S,P),println(port=P),sleep(15),rserver_stop(S).

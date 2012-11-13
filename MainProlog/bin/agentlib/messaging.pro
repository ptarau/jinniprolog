/*  agent message exchanger hub: sender and reciever 
    wait until rendez-vous takes place
*/

out_mes(To,Mes):-server_port=>From,ask_broker(out_term(From,To,Mes)).

in_mes(From,Mes):-server_port=>To,ask_broker(in_term(From,To,Mes)).

rd_mes(From,Mes):-server_port=>To,ask_broker(rd_term(From,To,Mes)).

/*

window 1

?- new(agent(jane),P),P:in_mes(joe,X).
.... % waiting for mes
X = hi

window 2

?- new(agent(joe),P),P:out_mes(jane,hi).

*/

%@@
% COORDINATION SERVICES 

new_named_service(Name,X<=G):-ask(and(topcall(G),Name<=X)).

call_named_service(Name,X=>G):-ask(and(Name=>X,topcall(G))).

% barriers

/*
   An Barrier ensures a number of threads wait jointly. When all finish,
   a Runnable action is executed. The barrier is usable only once and
   is expected to be garbage collected after it accomplishes its mission.
*/

new_named_barrier(Name,Max):-
  new_named_service(Name,B<=new_barrier(Max,B)).
  
named_barrier_arrive(Name):-
  call_named_service(Name,B=>barrier_arrive(B)).
  
named_barrier_stop(Name):-
  call_named_service(Name,B=>barrier_stop(B)).

/* hubs: M to N producer consumer pattern with
   implicit coordination.
   A producer suspends until the consumer takes away
   the content of the hub.
   A consumer suspends until the hub has content.
*/

new_named_hub(Name):-
  new_named_service(Name,H<=hub(H)).

new_named_hub(Name,TimeOutMsec):-
  new_named_service(Name,H<=hub_ms(TimeOutMsec,H)).  
  
named_hub_get(Name,Term):-
  call_named_service(Name,H=>hub_collect(H,Term)).  
  
named_hub_set(Name,Term):-
  call_named_service(Name,H=>hub_put(H,Term)).    
  
named_hub_stop(Name):-
  call_named_service(Name,H=>hub_stop(H)). 

% and_hubs: a consumer is suspended until N producers provide their content

new_named_and_hub(Name,Max):-
  new_named_service(Name,H<=new_and_hub(Max,H)).

named_and_hub_set(Name,I,Term):-
  call_named_service(Name,H=>and_hub_set(H,I,Term)).  
  
named_and_hub_add(Name,Term):-
  call_named_service(Name,H=>and_hub_add(H,Term)). 

named_and_hub_get(Name,I,Term):-
  call_named_service(Name,H=>and_hub_get(H,I,Term)). 

named_and_hub_remove(Name,Term):-
  call_named_service(Name,H=>and_hub_remove(H,Term)). 
 
named_and_hub_all(Name,Terms):-
  call_named_service(Name,H=>and_hub_all(H,Terms)). 
       
named_and_hub_stop(Name):-
  call_named_service(Name,H=>and_hub_stop(H)). 

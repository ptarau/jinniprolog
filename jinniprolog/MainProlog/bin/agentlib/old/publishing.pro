% TRACER

pst(_). % no trace
%pst(T):-println(tracing=T). % trace for debugging

% EXAMPLES

test_publishing:-
  publish_computed(I,for(I,1,25),cool_content).

test_subscribing:-
  Channel=cool_content,
  subscribe_to(Channel,println).

test_fpublishing:-
  publish_file(char_of,peer).

test_fsubscribing:-
  subscribe_to_file(char_of,peer,put_until_eof).

put_until_eof(Char):-integer(Char),Char>0,!,put(Char).

test_dpublishing:-
  publish_dir(char_of,'.').

test_dsubscribing:-
  subscribe_to_file(char_of,'./peer.pro',put_until_eof).


% ADVERTIZE-PUBLISH-SUBSCRIBE PATTERN

/* API:

advertize(Channel): this agent is posted on the registrar 
  as a publisher for Channel
  
get_advertizer(Channel,OwnerUID):
  queries the registrar about channels and their owners

wait_for_advertizer(Channel,OwnerUID):
  waits until an agent adevertizes Channel, in which
  case it obtains OwnerUID.
  
subscribe(ProviderUID,Channel,CallBack): subscribes this agent
  as a receiver of content from ProviderUID on Channel using CallBack
  as a content processor

subscribe_to(Channel,CallBack): subscribes this agent
  as a receiver of content from Channel using CallBack
  as a content processor
  
unsubscribe(ProviderUID,Channel,CallBack):
  removes this agent from the set of receivers
  of content from ProviderUID on Channel using
  CallBack as a content processor
  
publish(Channel,Content): 
  publish new content on a channel - which means distributing it to
  all subscribers through their content processors

EXTENDED API:

publish_computed(Answer,Goal,Channel):
  publishes each Answer for Goal on Channel.

publish_file(Reader,File):
  Iterates by backtracking over File, using Reader (i.e. term_of, char_of)
  and publishes the content on a Channel that keeps track of the reader used
  (a form of strong typing). The subscriber should provide the same reader
  to prove it is aware of the structure of the input stream.

subscribe_to_file(Reader,File,CallBack):
   Applies CallBack to each item published by the Publisher.
   The Subscriber confirms that it knows what Reader has been used,
   as a from of type safety.
   
USE CASE:

  an agent advertizes a Channel and starts publishing content on it
  
  a few other agents query the registrar and find the channel and its
  owner
  
  the agents that find the channel interesting subscribe to it (they
  can choose to wai until a given Channel becomes available.
  
  after a while various agents unsubscribe and stop receiving 
  content from the channel
 
PLEASE READ THE ACTUAL CODE FOR EACH PREDICATE BEFORE USING THIS
FOR UNDERSTANDING DETAILS ON PERFORMANCE AND BEHAVIOR.
*/


publish_computed(Goal):-
  functor(Goal,F,_),
  Channel=F,
  Answer=Goal,
  publish_computed(Answer,Goal,Channel).

publish_computed(Answer,Goal,Channel):-
  nonvar(Goal),nonvar(Channel),
  advertize(Channel),
  wait_for_subscribers(Channel),
  Content=Answer,
  (metacall(Goal),
    pst(publishing=Content),
    publish(Channel,Content),
   fail
  ; pst(end_publishing)
  ).

subscribe_to_computed(Channel,CallBack):-
  wait_for_advertizer(Channel,PublisherUID),
  subscribe(PublisherUID,Channel,CallBack).
  
publish_dir(Dir):-publish_dir(term_of,Dir).

publish_dir(Reader,Dir):-
 foreach(
   ( dir_has_file(Dir,FileName),
     pst(dir_has_file(Dir)=FileName),
     namecat(Dir,'/',FileName,SourceF)
   ),
   ( sleep(1),
     bg(publish_file(Reader,SourceF))
   )
 ).
 
publish_char_file(File):-
  Reader=char_of,
  publish_file(Reader,File).
  
publish_token_file(File):-
  Reader=token_of,
  publish_file(Reader,File).

publish_sentence_file(File):-
  Reader=sentence_of,
  publish_file(Reader,File).
    
publish_term_file(File):-
  Reader=term_of,
  publish_file(Reader,File).
    
publish_file(Reader,File):-
  pst(publishing_file=File),
  %find_close_file(File,AbsFile),
  AbsFile=File,
  to_channel(Reader,File,Channel),
  advertize(Channel),
  wait_for_subscribers(Channel),
  uid=>Me,pst(publisher:Me),
  pst(advertized=>Channel:AbsFile),
  Closure=..[Reader,AbsFile,Content],
  foreach(
    Closure,
    publish(Channel,Content)
  ),
  publish(Channel,end_of_file).

to_channel(Reader,File,channel(Reader,File)).
  
advertize(Channel):-
  ground(Channel),
  !,
  uid=>Me,
  ask_registrar(new_ad(publishing(Channel,Me))).
advertize_channel(Channel):-
  errmes(channel_must_be_ground,Channel).
  
get_advertizer(Channel,OwnerUID):-
   ground(Channel),
   !,
   ask_registrar(get_ad(publishing(Channel,OwnerUID))).
get_advertizer(ChannelPattern,OwnerUID):-
   G=publishing(ChannelPattern,OwnerUID),
   ask_registrar(get_all_ads(G,Gs)),
   member(G,Gs).

wait_for_advertizer(Channel,OwnerUID):-
   ground(Channel),
   %ask_registrar(wait_for_ad(publishing(Channel,OwnerUID))),
   repeat,
     sleep(1),
     %println(waiting_for=Channel),
     get_advertizer(Channel,OwnerUID),
   !.

wait_for_subscribers(Channel):-wait_for_n_subscribers(Channel,1,100000000).
  
wait_for_n_subscribers(Channel,N,TimeOut):-
  for(_,1,TimeOut),
    sleep(1),
    count_answers(hash_get(Channel,subscriber(_,_)),K),
    pst(subscriber_count=>K),
  K>=N,
  !.
         
/*
  Notifies all current subscribers about Content
  using their specific callbacks.
*/  
publish(Channel,Content):-
  foreach(
    hash_get(Channel,subscriber(You,F)),
    i_push(You,F,Content)
  ).
    
i_push(You,F,Content):-
    pst(pushing=>F:Content=>You),
    G=..[F,Content],
    ask_uid(You,G).

i_clear(Channel):-
  hash_clear(Channel).

i_clear_all:-
  hash_clear.
      
subscribe_me(Channel,Me,CallBack):-
  hash_put(Channel,subscriber(Me,CallBack)).

unsubscribe_me(Channel,Me,CallBack):-
  hash_rm(Channel,subscriber(Me,CallBack)).
  
  
/* adds this agent to the observer list of
   Channel of agent specified by UID "You"
*/
subscribe(You,Channel,CallBack):-
  uid=>Me,
  pst(subscribing=>You:Channel),
  ask_uid(You,subscribe_me(Channel,Me,CallBack)).

subscribe(You,Channel):-subscribe(You,Channel,println).

unsubscribe(You,Channel,CallBack):-
  uid=>Me,
  ask_uid(You,unsubscribe_me(Channel,Me,CallBack)).

unsubscribe(You,Channel):-unsubscribe(You,Channel,println).

/*
  Waits for advertizer for Channel and then subscribes to it,
  providing CallBack as predicate it will call on each
  item it gets from the Publisher.
*/
subscribe_to(Channel,CallBack):-
  wait_for_advertizer(Channel,PublisherUID),
  subscribe(PublisherUID,Channel,CallBack).

subscribe_to(Channel):-subscribe_to(Channel,println).

subscribe_to_term_file(File):-
  Reader=term_of,
  CallBack=pp_clause,
  subscribe_to_file(Reader,File,CallBack).

subscribe_to_char_file(File):-
  Reader=char_of,
  CallBack=println,
  subscribe_to_file(Reader,File,CallBack).

subscribe_to_token_file(File):-
  Reader=token_of,
  CallBack=println,
  subscribe_to_file(Reader,File,CallBack).

subscribe_to_sentence_file(File):-
  Reader=sentence_of,
  CallBack=println,
  subscribe_to_file(Reader,File,CallBack).
      
subscribe_to_file(Reader,File,CallBack):-
  to_channel(Reader,File,Channel),
  subscribe_to(Channel,CallBack).
 
% TOOLS

% tasks by uid

uid2host(UIDnhp,Host):-arg(2,UIDnhp,Host).

uid2port(UIDnhp,Port):-arg(3,UIDnhp,Port).

% ask an agent specified by its UID to do G

ask_uid(UID,G):-Pwd=none,ask_uid(UID,Pwd,G).

ask_uid(UID,Pwd,G):-
  uid2host(UID,H),
  uid2port(UID,P),
  ask_agent(H,P,Pwd,G,G,the(G)).
  
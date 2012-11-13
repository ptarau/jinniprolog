
Some notes on agents:

Our basic building blocks (extensible by users) are defined in
peer.pro. tester.pro shows some tests - from an "outsider's perspective".
i.e. tester.pro does not extend any other agents classes.

Example for starting peer agents, using:

new(Class,Inst): creates a new instance from Prolog file <Class>.pro

peerdemo:-
  % peer creates registrar that serves the uid if absent
  new(peer,P),P:uid=>U,println(uid(p)=U),
  % peer Q senses registrar and request uid - if not, launch new registrar
  new(peer,Q),Q:uid=>V,println(uid(q)=V),
  % Q creates a content hub 
  Q:new_named_hub(myhub),
  % Q is the consumer agent with 1 or more threads (takes away the content)
  Q:bg((named_hub_get(myhub,X),println(got=X))),
  %println('0_sec'),sleep(10),println('10_sec'),
  %Now we launch P as a producer for myhub
  bg(P:named_hub_set(myhub,hello)),
  % launch other agents doing similar stuff
  bg(launch_agents(10)).
 
PUBLISH/SUBSCRIBE

- a mechanism for agents to start M to N consumer/producer interactions
  on a given Channel - without knowing each other's location info

see more in agentlib/publishing.pro

Some of the agent classes can be seen as abstract - in the sense of
simply providing some functionality meant to be embedded in 
actual agents, for instance:

publishing.pro: implements the publish/subscribe API

coordination.pro: provides coordination primitives

tester.pro: shows some testing from an outsider's perspective



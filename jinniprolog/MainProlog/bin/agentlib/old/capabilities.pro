% adds a list of capabilities describing what 
% a predicate (seen as a service)can do
add_capabilities(Predicate,ListOfAttributes):-
  add_associations(Predicate,ListOfAttributes).

% adds a list of capabilities describing what 
% a predicate (seen as a service)can do
add_capability(Predicate,Attribute):-
  add_association(Predicate,Attribute).

% Given a list of capabilities describing what 
% a predicate (seen as a service) should do,
% this finds a set of matching predicates.
% It is a good idea to check with this that the set of attributs
% identify a predicate uniquely.
% In this case you can call the predicate "intensionally" by using
% an extension to our Hilog syntax [Attr1,Attr2..]@(Arg1,Arg2) which will
% generate dynamically a call: P(Arg1,Ar2...) where P is the unique
% predicate providing the capabilities - and produce an error in case
% none, or more than one qualify. It also works with closures of the form
% F(A,B...) insted of predicates alsthough this is seldomly needed.

get_capabilities(AttributeList,Predicate):-
  findall(P,association_to_closure(AttributeList,P),Predicate).

% lists all capabilities defined by this agent
list_capabilities:-list_associations.


/* POSSIBLE FUTURE DEVELOPMENT

% capabilities puzzle: from a set of "can do" "wants done" rules
% match and define the workflow for a collection of agents

can_do((a;b;c;d)).
wants_done((x,y,z)).

seen as:

a;b;c;d:-x,y,z.

% these rules in disjunctive datalog will be grounded and then
% sent to a SAT solver. A the resulting model 

a conjunction of the form

  p,q,r,...
  
can be seen as an agent configuration - that is consistent with the agents capabilities and
goals, assuming they are truthful. To achive the "plan", the model is used to generate calls.
*/

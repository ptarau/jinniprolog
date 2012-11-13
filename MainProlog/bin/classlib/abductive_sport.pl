:-[abductive].

% user's definitions

sports(winter) --> ?snows, ?sunny.
sports(summer) --> cool.

cool--> ?rains.
cool--> ?breeze.

% user's constraints

impossible --> ?rains,?snows.
impossible --> ?rains,?sunny.

% user's goal

go:-abduce(sports(_When)).

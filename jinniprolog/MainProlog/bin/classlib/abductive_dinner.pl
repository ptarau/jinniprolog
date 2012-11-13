:-[abductive].

% constraints

impossible --> ?wine(red),?fish.
impossible --> ?wine(white),?steak.
impossible --> ?wine(_),?driving.

necessary-->light_food ; exercise.

% rules

dinner-->food,drink,locomotion.

locomotion-->?driving ; ?walking.

drink-->{Color=red;Color=white},?wine(Color) ; ?water.

food-->?steak ; ?fish.

light_food-->?fish.

exercise-->?walking.

% test

go:-abduce(dinner).

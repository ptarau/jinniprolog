:-[abductive].

% definitions

is_sensed--> ?is_seen,?is_heared;?is_seen,?is_touched;?is_heared.

% constraints

majority(0.5)--> ?is_seen,?is_heared,?is_touched.

% goal

go:-abduce(is_sensed).

% end

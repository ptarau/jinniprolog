% :-[qbug].

/* difference in semantcs */

%abug(X):-bug<=X,bg(run_server).
abug(X):-bug<=X,rli_start_server.

b(X):-a(X).

a(1).
a(2).

/* $$ different beahvior */
% bugtest:-remote_run(bug=>X).
bugtest:-rli_call(bug=>X).
  
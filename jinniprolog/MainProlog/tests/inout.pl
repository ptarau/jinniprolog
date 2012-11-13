bpbug:- %there,
     for(I,1,100000),
     out(a(pino)),
     println(I),
     sleep(1),
     in(a(pino)),
     fail.

go:-
     for(I,1,100000),
     remote_run(out(a(pino))),
     % println(I),
     remote_run(in(a(pino))),
     fail.

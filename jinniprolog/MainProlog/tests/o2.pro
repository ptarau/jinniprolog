o2:-go.

o2(X):-println(o2:X).

go:-is_compiled(o1),println(compiled=o1),fail.
go:-println(o2).

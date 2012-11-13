% finite functions expressed as equality 
% mapping between 2 lists of logic variables

functions_from([],_).
functions_from([V|Vs],Us):-member(V,Us),functions_from(Vs,Us).

% finite partial functions expressed as equality 
% mapping between 2 lists of logic variables

partial_functions_from([],_).
partial_functions_from([V|Vs],Us):-maybe_member(V,Us),partial_functions_from(Vs,Us).

maybe_member(_,_).
maybe_member(X,Xs):-member(X,Xs).


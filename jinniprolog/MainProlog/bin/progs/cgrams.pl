% implements Okhotin's conjunctive grammars (easy extension to Boolean grammars!)
% a^n b^n c^n

% xs-->xa,xb & xc,xd.
xs(S0,Sn):-
 xa(S0,S1),xb(S1,Sn), 
 xc(S0,S2),xd(S2,Sn).

xa-->[a],xa;[].
xb-->[b],xb,[c];[].

xc-->[a],xc,[b];[].
xd-->[c],xd;[].

go:-xs([a,a,b,b,c,c],[]).


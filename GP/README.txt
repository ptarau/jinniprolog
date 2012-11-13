Combinatorics in Prolog
===================================

On a Windows PC type one of the following

go.bat
jgo.bat
rgo.bat

At the prolog prompt, type queries
from comb.pro

The code is portable, except for some step counting functions 
that use BinProlog and Jinni specifics like change_arg and if_any.
For a quick port to another Prolog just comment that out in comb.pro.

For structures using bitstrings, this version supports 
at most 4 variables as it only uses
16 bits out of BinProlog's 29 bit integers. In a Prolog
with arbitrary length ints this should work with more,
provided that the problem is tractable computationaly.

Type rgo.bat to display various things using
display server agents.

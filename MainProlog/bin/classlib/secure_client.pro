:-[client].
:-[sealed_term].

/*
USAGE:

?- new(secure_client(localhost,2003,tweety),C),C:ask(println(hello)),C:disconnect.
C = '$instance'('$object'(859),1) ;
no

*/
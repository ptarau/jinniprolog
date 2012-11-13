/*

 These are hooks in the Prolog Transport layer
 (see net.pl and Transport.java)
 When this file is compiled, they modify the 
 Transport layer's behavior such that they 
 terms written to sockets are automatically incripted and descripted.

 Using identity operations will result in reading and writing
 which serialize terms but which do not encrypt them.

*/

term_encoder(T,T).

term_decoder(T,T).


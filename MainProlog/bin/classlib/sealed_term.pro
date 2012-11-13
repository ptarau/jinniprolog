:-[crypto].

sealed_term:-
  sealed_term(none).
  
sealed_term(Password):-
  password<=Password.
  
seal(Term,SealedObject):-
  password=>Password,
  seal(Password,Term,SealedObject).
  
unseal(SealedObject,Term):-
  password=>Password,
  unseal(Password,SealedObject,Term).
   
seal(Password,Term,SealedObject):-
  get_encrypt_mode(Mode),
  new_cipher(Mode,Password,Cipher),
  new_java_object('javax.crypto.SealedObject'(Term,Cipher),SealedObject).
  
unseal(Password,SealedObject,Term):-
  % get_decrypt_mode(Mode),new_cipher(Mode,Password,Cipher),
  make_key(Password,Key),
  invoke_java_method(SealedObject,getObject(Key),Term).

test:-
  password=>Password,
  T1=f(g(11,a,s(X,Y)),h(Y,X,3.14)),
  seal(Password,T1,Sealed),
  unseal(Password,Sealed,T2),
  println(T1=T2),T1=T2.

/*
 These are hooks in the Prolog Transport layer
 (see net.pl and Transport.java)
 When this file is compiled, they modify the 
 Transport layer's behavior such that they 
 terms written to sockets are automatically incripted and descripted.

 These predicates serialize/deserialize and encrypt/decrypt terms as Sealed objects  
*/

term_encoder(PlainT,CipherT):-seal(PlainT,CipherT).

term_decoder(CipherT,PlainT):-unseal(CipherT,PlainT).

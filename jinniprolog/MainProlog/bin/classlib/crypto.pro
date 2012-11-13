:-[arrays].

term_encoder1(T,S2):-
  swrite(T,S1),to_bytes(S1,Bs),
  encrypt(eureka,Bs,Cs),
  from_bytes(Cs,S2),
  println(T=>S2).
  
term_decoder1(S1,T):-
  to_bytes(S1,Cs),
  decrypt(eureka,Cs,Bs),
  from_bytes(Bs,S2),
  println(string=S2),
  sread_term(S2,T).

make_key(Password,SecretKey):-
  to_chars(Password,Chars),
  new_java_object('javax.crypto.spec.PBEKeySpec'(Chars),KeySpec),
  call_java_class_method('javax.crypto.SecretKeyFactory',
     getInstance('PBEWithMD5AndDES'),Factory),
  invoke_java_method(Factory,generateSecret(KeySpec),SecretKey).  

make_params(Params):-
  to_bytes('nopepper',Salt),  
  new_java_object('javax.crypto.spec.PBEParameterSpec'(Salt,20),Params).
  
make_cipher(Cipher):- 
  call_java_class_method('javax.crypto.Cipher',
     'getInstance'('PBEWithMD5AndDES'),Cipher).
 
init_cipher(Cipher,Mode,Key,Params):-
  % println(here(Cipher,Mode,Key)),
  invoke_java_method(Cipher,init(Mode,Key,Params),_).
    
get_encrypt_mode(EM):-
  get_java_class_field('javax.crypto.Cipher','ENCRYPT_MODE',EM).

get_decrypt_mode(DM):-
  get_java_class_field('javax.crypto.Cipher','DECRYPT_MODE',DM).

new_cipher(Mode,Password,Cipher):-
  make_cipher(Cipher),
  make_key(Password,Key),
  make_params(Params),
  init_cipher(Cipher,Mode,Key,Params).

run_cipher(Mode,Password,InputBytes,OutputBytes):-
  new_cipher(Mode,Password,Cipher),
  apply_cipher(Cipher,InputBytes,OutputBytes).
  
apply_cipher(Cipher,Input,Output):-
  invoke_java_method(Cipher,doFinal(Input),Output).

encrypt(Password,PlainTextBytes,CipherTextBytes):-
  get_encrypt_mode(Mode),
  run_cipher(Mode,Password,PlainTextBytes,CipherTextBytes).

decrypt(Password,CipherTextBytes,PlainTextBytes):-
  get_decrypt_mode(Mode),
  run_cipher(Mode,Password,CipherTextBytes,PlainTextBytes).

test:-
  Password=eureka,
  to_bytes('the dog barks',PlainTextBytes),
  show_bytes(PlainTextBytes),
  encrypt(Password,PlainTextBytes,CipherTextBytes),
  show_bytes(CipherTextBytes),
  decrypt(Password,CipherTextBytes,OtherTextBytes),
  show_bytes(OtherTextBytes).
  
to_bytes(String,Bytes):-
  invoke_java_method(String,'getBytes',Bytes).

to_chars(String,Chars):-
   invoke_java_method(String,'toCharArray',Chars).        
   
from_bytes(Bytes,String):-
   new_java_object('java.lang.String'(Bytes),String).
   
byte_at(I,Bytes,B):-
  call_java_class_method('java.lang.reflect.Array',
     'getByte'(Bytes,I),OB),
  invoke_java_method(OB,intValue,B).  

byte_element_of(Bytes,B):-
  get_length(Bytes,N),
  Last is N-1,
  for(I,0,Last),
  byte_at(I,Bytes,B).
  
char_at(I,Chars,C):-
  call_java_class_method('java.lang.reflect.Array',
     'getChar'(Chars,I),OC),
  invoke_java_method(OC,toString,C).

get_length(Array,L):-
  call_java_class_method('java.lang.reflect.Array',
     'getLength'(Array),L).
            
show_bytes(Bs):-
  from_bytes(Bs,S),
  println(S).


xsave:-
  get_class_name(Name),
  namecat(Name,'.bin','',File),
  xsave(File).

xload:-
  get_class_name(Name),
  namecat(Name,'.bin','',File),
  xload(File).
    
xsave(File):-
 this_db(Db),
 db_xsave(Db,File).

xload(File):-
  this_db(Db),
  db_xload(File,Db).
  
db_xsave(Db,File):-db_xsave(Db,File,none).

db_xload(File,Db):-db_xload(File,Db,none).
    
db_xsave(Db,File,Pwd):-
  queue_create(IMQ),handle2object(IMQ,MQ),
  name(Pwd,Ps),invoke_java_method(MQ,enq(Ps),_),
  ( db_get_pred(Db,F/N),
    functor(H,F,N),
    val(Db,H,IQ),handle2object(IQ,Q),
    invoke_java_method(MQ,enq(F/N-Q),_),
    fail
  ; object2file(MQ,File),
    delete_java_object(MQ)
  ).

db_xload(File,Db,Pwd):-
  db_clean(Db),
  file2object(File,MQ),
  ( invoke_java_method(MQ,deq,Ps),name(Pwd,Ps)->true
  ; errmes('bad password in saved database',file(File))
  ),
  handle2object(IMQ,MQ),
 
  % queue_list(IMQ,Es),
  queue_size(IMQ,S),Max is S-1,for(I,0,Max), invoke_java_method(MQ,elementAt(I),E),
    % member(E,Es),
    E=F/N-Q,handle2object(IQ,Q),
    functor(H,F,N),db_add_dynamic(Db,H),
    def(Db,H,IQ),
  fail.
db_xload(_File,_Db,_Pwd).

xwrap(T,O):-
 queue_create(Q),
 queue_add(Q,T),
 handle2object(Q,O).
 
xunwrap(O,T):-
  handle2object(Q,O),
  queue_pop(Q,X),
  T=X.
 
% sets on/off zipping of saved states
compress_on:-
  call_java_class_method(
      'prolog.core.Transport',
      compressOn,_
  ).
compress_off:-
  call_java_class_method(
      'prolog.core.Transport',
      compressOff,_
  ).
  
% serialize object
object2file(O,File):-
  call_java_class_method(
      'prolog.core.Transport',
      toFile(File,O),_
  ).
 
% rebuild serialized object  
file2object(File,O):-  
  call_java_class_method(
    'prolog.core.Transport',
    fromFile(File),O
  ).

% serialize object compatibile with java.beans XML codec
object2xml(O,File):-
  new_java_object('java.io.FileOutputStream'(File),OF),
  new_java_object('java.io.BufferedOutputStream'(OF),BF),
  new_java_object('java.beans.XMLEncoder'(BF),E),
  invoke_java_method(E,writeObject(O),_),
  invoke_java_method(E,close,_),
  foreach(member(JO,[OF,BF,E]),delete_java_object(JO)). 
 
% rebuild serialized XML object  
xml2object(File,O):-
  %new_java_object('java.io.FileInputStream'(File),IF),
  call_java_class_method('prolog.kernel.JavaIO',url_or_file(File),IF),
  new_java_object('java.io.BufferedInputStream'(IF),BF),
  new_java_object('java.beans.XMLDecoder'(BF),D),
  invoke_java_method(D,readObject,O),
  invoke_java_method(D,close,_),
  foreach(member(JO,[IF,BF,D]),
  delete_java_object(JO)).


/* Future ISO prolog compatibility layer.

   For now - mostly small tricks to run as many
   Prolog programs written for other systems, as possible.
   
   TODO: enhance parser
*/

between(Min,Max,Min):-Min=<Max.
between(Min,Max,I):-Min<Max,Min1 is Min+1,between(Min1,Max,I).

xor(X,Y,Z):- '#'(X,Y,Z).

predicate_property(H,(dynamic)):-(nonvar(H)->is_dynamic(H);get_pred(F/N),functor(H,F,N)).
predicate_property(H,(built_in)):-
  ( nonvar(H)->is_compiled(H)
  ; bbuiltin(HC,_,_),functor(HC,F,N1),N is N1-1,functor(H,F,N)
  ).
  
% stream I/O

fopen(Fname,'r',r(Reader)):-
  call_java_class_method('prolog.kernel.JavaIO',toReader(Fname),Reader).
fopen(Fname,'w',w(Writer)):-
  call_java_class_method('prolog.kernel.JavaIO',toWriter(Fname),Writer).

fgetc(r(Reader),C):-invoke_java_method(Reader,read,C).

fclose(r(Reader)):-invoke_java_method(Reader,close,_),delete_java_object(Reader).
fclose(w(Writer)):-invoke_java_method(Writer,close,_),delete_java_object(Writer).

fputc(w(Writer),C):-invoke_java_method(Writer,super_write(C),_).

fnl(w(Writer)):-invoke_java_method(Writer,println,_).

fprintln(w(Writer),T):-swrite(T,S),invoke_java_method(Writer,println(S),_).

fprintf(w(Writer),Format,ArgList):-
  fix_format_arg(Format,FormatSpec),
  list_to_array(ArgList,Args),
  invoke_java_method(Writer,printf(FormatSpec,Args),_).

fix_format_arg(Format,Fixed):-atom(Format),!,Fixed=Format.
fix_format_arg(ListFormat,Fixed):-atom_codes(Fixed,ListFormat).

fprintf_test:-
  fopen('temp.txt','w',F),
  X is 7/3,
  fprintf(F,"%s %g",[hello,X]),fnl(F),
  fclose(F),
  fopen('temp.txt','r',F1),
  repeat,
    fgetc(F1,C),
    ( C=:= -1 -> !
    ; put(C),
      fail
    ),
    fclose(F1).
    
fcopy_test(From,To):-
  fopen(From,'r',Input),
  fopen(To,'w',Output),
  repeat,
    fgetc(Input,C),
    ( C =:= -1 -> !
    ; fputc(Output,C),
      fail
    ),
  fclose(Input),
  fclose(Output).

rfopen(FName,FHandle):-
  new_java_object('java.io.RandomAccessFile'(FName,'rw'),FHandle).

rfcall(FHandle,MethArgs,Result):-
  invoke_java_method(FHandle,MethArgs,Result).

rfget(FHandle,Byte):-rfcall(FHandle,read,Byte).

rfseek(FHandle,NBytes):-rfcall(FHandle,seek(NBytes),_).

rfput(FHandle,Byte):-rfcall(FHandle,writeByte(Byte),_).

rfskip(FHandle,NBytes):-rfcall(FHandle,skipBytes(NBytes),_).

rfclose(FHandle):-rfcall(FHandle,close,_).

rfappend(FHandle,Byte):-
  rfcall(FHandle,length,L),
  rfseek(FHandle,L),
  rfput(FHandle,Byte).
  
rftest:-
  rfopen('rfile.txt',F),
  rfseek(F,0),
  [A,B,C]="abc",
  rfput(F,A),
  rfappend(F,B),
  rfappend(F,C),
  rfclose(F).
   

% end

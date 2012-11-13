bug:-
  asserta(memqa(l,p,[a,b],[c,d],s,s)),
  asserta(memqa(l,p,[a,b,c],[c,d,e],sa,sb)),
  fail
; 
  findall(IWs,asserted(memqa(Login,Password,IWs,_OWs,_Stage1,_Stage2)),Wss),
  ttyprint('!!!!!!!!!!!!!here'(Login,Password)=Wss).

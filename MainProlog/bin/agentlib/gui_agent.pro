% :-[client]. //// deprecated

:-[agent].

gui_agent(PortName):-gui_agent(PortName,true).

gui_agent(PortName,Goal):-gui_agent(PortName,PortName,Goal).

gui_agent(WinName,PortName,Goal):-gui_agent(new_console,WinName, PortName,Goal). % or new_ide

/*
//gui_agent(Gui,WinName, PortName,InitialGoal):-
//  Goal=bg(and(server_port<=PortName,InitialGoal)),
//  run_rli_gui(Gui,WinName,PortName,Goal),
//  server_port<=PortName,
//  rli_call(PortName,server_port<=PortName).
*/

% ////
gui_agent(Gui,WinName, PortName,InitialGoal):-
  InitPort=(server_port<=PortName),
  call(InitPort),
  Goal=bg(and(InitPort,InitialGoal)),
  run_rli_gui(Gui,WinName,PortName,Goal).
  
/*
% deprecated
gui_agent(Gui,Goal,MyPort,BrokerHost,BrokerPort,Password):-
  gui_agent(Gui,MyPort,Goal,MyPort,BrokerHost,BrokerPort,Password).

gui_agent(Gui,WinName,Goal,MyPort,BrokerHost,BrokerPort,Password):-
  agent(run_gui(Gui,WinName,Goal),MyPort,BrokerHost,BrokerPort,Password).

*/

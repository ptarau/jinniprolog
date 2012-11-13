:-println('type demo3d to start prolog3d 2.0x demo').

version3d('2.0x').

demo3d:-reuse_top_window(yes),ldemo3d,refs3d,close_world. % refs of file prolog3d.pl
  
ldemo3d:-gr,shapes3d,gh,gr1,gr2,modeltest.
sdemo3d:-reuse_top_window(yes),jftest,gr,morphtest,close_world.

set_applet:-call_java_class_method('prolog3d.Params',setApplet,_).
is_applet:-call_java_class_method('prolog3d.Params',isApplet,T),is_true(T).

/**
Prolog3D API
*/

% creates a new 3D world in which various 3D agents are placed
new_world(U):-
  new_java_class('prolog3d.Prolog3D',C),
  new_java_object(C,void,U).

new_world(Frame,U):-
  new_java_class('prolog3d.Prolog3D',C),
  new_java_object(C,args(Frame),U).

% display the world in frame of given size     
show_world(U,Width,Height):-
  invoke_java_method(U,showWorld(Width,Height),_).

% displays a default size world
show_world(U):-show_world(U,500,500).

% prints or converts the world to PNG file in frame of given size     
print_world(U,Width,Height):-
  invoke_java_method(U,printWorld(Width,Height),_).

% prints or converts the world to PNG file of default frame size
print_world(U):-print_world(U,300,300).

% adds a a panel with controls to a world
add_controls(U):-invoke_java_method(U,addControls,_).
add_output(U,TextArea):-invoke_java_method(U,addOutput(TextArea),_).

add_panel(U,P):-add_panel(U,'North',P).
add_panel(U,Where,P):-invoke_java_method(U,addPanel(Where),P).

% destroys world U  
stop_world(U):-
  invoke_java_method(U,stopWorld,_),
  delete_java_object(U).

close_world:-
  new_world(U),
  show_world(U),
  close_world(U).
  
close_world(U):-
  reuse_top_window(no),
  stop_world(U).
  
% sets the coordinates of the 'camera' the world is viewed from    
set_view(U,X,Y,Z):-
  invoke_java_method(U,setView(X,Y,Z),_).

% sets a given agent as a 'holder' of the 'camera' that will follow its transforms   
set_holder(U,A):-
  invoke_java_method(U,setHolder(A),_).

remove_holder(U):-
  invoke_java_method(U,removeHolder,_).  

ide3d:-
  prolog_ide_world(U,'agent==>A,inc_x(A,0.5)',Out),
  add_text(Out,'Welcome to this ide3d demo!'),
  sphere(U,A),
  agent<==A,
  show_world(U,800,600),
  sleep(100000).

ide3dh:-
  prolog_ide_world(U,'halt',Out),
  add_text(Out,'Hypergraph in a 3D IDE demo'),
  new_cat(G),
  add_hyper_edge(G,ab,[a,b]),
  add_hyper_edge(G,bc,[b,c]),
  add_hyper_edge(G,ca,[c,a]),
  add_hyper_edge(G,efg,[e,f,g,h]),
  add_hyper_edge(G,hi,[h,i]),
  model_graph(U,500,G),
  show_world(U,800,600),
  sleep(100000).


ide3dt:-
  prolog_ide_world(U,'halt',Out),
  add_text(Out,'Tree in a 3D IDE demo'),
  new_cat(G),
  
  set_prop(G,a,data,v_a),
  set_morphism(G,a,b,data,1),
  set_morphism(G,a,c,data,2),
  set_morphism(G,b,d,data,3),
  set_morphism(G,b,e,data,4),
  set_morphism(G,b,f,data,5),
  set_morphism(G,e,g,data,6),
  set_morphism(G,f,h,data,7),
  set_morphism(G,f,i,data,8),
  set_morphism(G,f,j,data,9),
  
  draw_tree(U,500,G),
  show_world(U,800,600),
  sleep(100000).
    
prolog_ide_world(U):-prolog_ide_world(U,true,_OutWindow).

prolog_ide_world(U,Query,OutWindow):-
  prolog_ide_world('Prolog3D','East',Query,U,OutWindow).
  
prolog_ide_world(Title,Where,Goal, U,OutWindow):-
  %reuse_top_window(yes),
  new_frame(Title,border,F),
  new_world(F,U),
  add_panel(U,Where,P),
  new_ide(P,Goal,OutWindow),
  add_output(U,OutWindow),
  add_controls(U).
      
/************** AGENT3D API ********************/

% simple objects, 2D and 3D text and models represented as agents
% mostly vertex3d agents and edge3d agents

sphere(U,A):-shaped_agent(U,0,sphere,A).

point(U,A):-shaped_agent(U,1,point,A).

line(U,A):-shaped_agent(U,2,line,A).

tetra(U,A):-shaped_agent(U,3,tetra,A).

box(U,A):-shaped_agent(U,4,box,A).
box(U,R,G,B,T,A):-vertex3d(U,4, R,G,B,T, box,A).

color_cube(U,A):-shaped_agent(U,5,color_cube,A).

pyram(U,A):-shaped_agent(U,6,pyram,A).

frozen_shape(U,A):-shaped_agent(U,7,frozen_shape,A).

% complex objects and models

cone(U,A):-shaped_agent(U,8,cone,A).

cylinder(U,A):-shaped_agent(U,9,cylinder,A).

text2d(U,Text,A):-shaped_agent(U,10,Text,A).

text3d(U,Text,A):-shaped_agent(U,11,Text,A).

text2dno(U,Text,A):-shaped_agent(U,12,Text,A).

text3dno(U,Text,A):-shaped_agent(U,13,Text,A).

model3d(U,FileName,A):-shaped_agent(U,14,FileName,A).

body3d(U,FileName,A):-body3d(U,FileName,body3d,A).

body3d(U,FileName,Data,A):-
  invoke_java_method(U,addBody(FileName,Data),A).

body_set_focus(Body,PartName):-
  body_set_focus(Body,PartName,yes).

body_set_focus(Body,PartName,Yes):-
  invoke_java_method(Body,setFocus(PartName),Ok),
  ( 
    is_true(Ok)->Yes=yes;Yes=no
  ).

body_reset_focus(Body):-body_set_focus(Body,'$null',_).
  
morph3d(U,Shape3DFiles,Data,A):-
  FXs=..[shapes|Shape3DFiles],
  invoke_java_method(U,addMorph(FXs,Data),A).
  
set_bg(File):-set_java_class_field('prolog3d.Params',bgfile,File).

set_max_text(N):-set_java_class_field('prolog3d.Params','maxText',N).

color_bg(R,G,B):-call_java_class_method('prolog3d.Params',setBgColor(R,G,B),_).

set_interactive(Int):-set_java_class_field('prolog3d.Params',interactive,Int).

reuse_top_window(YesNo):-
  (YesNo=yes->B=true;B=false),
  to_boolean(B,TF),
  set_java_class_field('prolog3d.Params',reuseTopWindow,TF).


% creation of agents=object+behaviors

% adds an agent with given ShapeNumber Color and Data content
shaped_agent(U,ShapeNumber,Data,A):-  
  invoke_java_method(U,addVertex(ShapeNumber,'$null',Data),A).

% adds an agent with given Shape and Data content
vertex3d(U,Shape,Data,A):-
  invoke_java_method(U,addVertex(Shape,Data),A).

% adds an agent with given Shape RGB colors, Transparency and Data content
vertex3d(U,Shape, R,G,B,Trans, Data,A):-invoke_java_method(U,addVertex(Shape,R,G,B,Trans,Data),A).
   
% adds an edge agent - to be positioned by a LayoutEngine  
edge3d(U,From,To,Label,E):-
  invoke_java_method(U,addEdge(From,To,Label),E).

% adds an edge agent - to be positioned by a LayoutEngine  
mobile_edge3d(U,From,To,Label,E):-
  invoke_java_method(U,addMobileEdge(From,To,Label),E).

% adds an agent with given ShapeNumber Color and Data content
satellite(U,From,ShapeNumber,R,Data,A):-  
  invoke_java_method(U,addSat(From,ShapeNumber,'$null',R,Data),A).
    
% stops and discards an agent 
stop_agent(A):-
  invoke_java_method(A,stop,_),
  delete_java_object(A).   

% resets an agent in center of the world and default parameters
reset_agent(A):-invoke_java_method(A,reset,_).

% makes an agent invisible
hide_agent(A):-invoke_java_method(A,hideAgent,_).

% makes an agent visible again
show_agent(A):-invoke_java_method(A,showAgent,_).

% moves an agent A to coordinates X,Y,Z
move_to(A,X,Y,Z):-invoke_java_method(A,moveTo(X,Y,Z),_).

% rotates an agent A using 3 Euler angles X,Y,Z
rotate_to(A,X,Y,Z):-invoke_java_method(A,moveTo(X,Y,Z),_).

% scales an agent A uniformly to S
scale_to(A,S):-scale_to(A,S,S,S).
 
% scales an agent by different rations on each coordinate  
scale_to(A,X,Y,Z):-invoke_java_method(A,scaleTo(X,Y,Z),_).

% sets an agent's current postion on a give component to X,Y or Z
set_x(A,X):-invoke_java_method(A,setX(X),_).
set_y(A,Y):-invoke_java_method(A,setY(Y),_).
set_z(A,Z):-invoke_java_method(A,setZ(Z),_).

% increment an agent's current postion by X,Y or Z
inc_x(A,X):-invoke_java_method(A,incX(X),_).
inc_y(A,Y):-invoke_java_method(A,incY(Y),_).
inc_z(A,Z):-invoke_java_method(A,incZ(Z),_).

% rotates an agent by a give Euler angle X,Y or Z
inc_rot_x(A,X):-invoke_java_method(A,incRotX(X),_).
inc_rot_y(A,Y):-invoke_java_method(A,incRotY(Y),_).
inc_rot_z(A,Z):-invoke_java_method(A,incRotZ(Z),_).

% colors an agent based on a Simple object
set_color(A,R,G,B):-invoke_java_method(A,setColor(R,G,B),_).

% increments color components within 0..1
inc_red(A,X):-invoke_java_method(A,incRed(X),_).
inc_green(A,X):-invoke_java_method(A,incGreen(X),_).
inc_blue(A,X):-invoke_java_method(A,incBlue(X),_).

% sets the time after which the renderer will redisplay
% changes made to the state of the agent
set_wake_up(A,Tick):-invoke_java_method(A,setWakeUp(Tick),_).

% starts/stops automatic random animation for agent A  
auto(A,Int):-invoke_java_method(A,setAuto(Int),_).

% gets current X,Y or Z coordinate of an agent
get_x(A,R):-invoke_java_method(A,getX,R).
get_y(A,R):-invoke_java_method(A,getY,R).
get_z(A,R):-invoke_java_method(A,getZ,R).

% gets current X,Y or Z Euler angle rotation of an agent
get_rot_x(A,R):-invoke_java_method(A,getRotX,R).
get_rot_y(A,R):-invoke_java_method(A,getRotY,R).
get_rot_z(A,R):-invoke_java_method(A,getRotZ,R).

% gets current X,Y or Z scale factor of agent A
get_scale_x(A,R):-invoke_java_method(A,getScaleX,R).
get_scale_y(A,R):-invoke_java_method(A,getScaleY,R).
get_scale_z(A,R):-invoke_java_method(A,getScaleZ,R).

% gets the complete translation/rotation/scale of an agent
get_agent_state(A,[pos(X,Y,Z),rot(RX,RY,RZ),scale(SX,SY,SZ)]):-
  get_x(A,X),
  get_y(A,Y),
  get_z(A,Z),
  get_rot_x(A,RX),
  get_rot_y(A,RY),
  get_rot_z(A,RZ),
  get_scale_x(A,SX),
  get_scale_y(A,SY),
  get_scale_z(A,SZ).

% returns a string rrepresentation of agent A
agent_to_string(A,S):-invoke_java_method(A,toString,S).

/************* GRAPH LAYOUT API *******************/

% shows a Prolog3D canvas
run_layout(U,Radius):-invoke_java_method(U,runLayout(Radius),_).

% shows a Prolog3D canvas
draw_graph(RG):-draw_graph(RG,0).

draw_graph(RG,TimeOut):-draw_graph(RG,TimeOut,400,400,400).

% shows a Prolog3D canvas for a graph layout animation 
% for Timout msecs in a world with given 3D radius and screen dimensions
draw_graph(RG,TimeOut,Radius,Width,Height):-
  new_java_class('prolog3d.Prolog3D',C),
  invoke_java_method(C,drawGraph(RG,TimeOut,Radius,Width,Height),_).

draw_frozen_graph(RG):-draw_frozen_graph(RG,0).

draw_frozen_graph(RG,TimeOut):-draw_frozen_graph(RG,TimeOut,400,400,400).

draw_frozen_graph(RG,TimeOut,Radius,Width,Height):-
  new_java_class('prolog3d.Prolog3D',C),
  invoke_java_method(C,drawFrozenGraph(RG,TimeOut,Radius,Width,Height),_).

show3d(Cat):-show3d('Prolog3D',Cat).

show3d(Title,Cat):-show3d(Title,Cat,1000000).

% show a 3D model of a Cat object
show3d(Title,Cat,Timeout):-
  prolog_ide_world(U,'halt',Out),
  add_text(Out,Title),
  model_graph(U,500,Cat),
  %draw_tree(U,500,Cat),
  show_world(U,800,600),
  sleep(Timeout).

show_term(T):-show_term(T,30).

show_term(T,Timeout):-show_term(T,6,6,0,Timeout).

show_term(T,FType,CType,MaxText,Timeout):-
  set_max_text(MaxText),
  term_to_cat(T,FType,CType,C),
  draw_graph(C,Timeout),
  % show3d('PrologTerm',C,Timeout),
  true.
  
% show a 3D model in a 3D ide of a Cat object
model_graph(U,Radius,RG):-
  new_java_class('prolog3d.Prolog3D',C),
  invoke_java_method(C,catModel(U,Radius,RG),_).

% show a 3D model in a 3D ide of a Cat object assumed to represent a tree
draw_tree(U,Radius,RG):-
  new_java_class('prolog3d.Prolog3D',C),
  invoke_java_method(C,drawTree(U,Radius,RG),_).
    
/*********** MODEL ANIMATION API *************/

obj2agent(U,F,A):-
  model_dir(D),
  namecat('/OBJ/',D,'',MD),
  namecat(MD,F,'.obj',FName),
  model3d(U,FName,A),
  scale_to(A,0.5).

vrml2agent(U,F,A):-
  model_dir(D),
  namecat(D,'/VRML/','',MD),
  namecat(MD,F,'.wrl.gz',FName),
  println(trying=FName),
  model3d(U,FName,A).

jf2agent(U,F,A):-
  model_dir(MD),
  namecat(MD,F,'.j3f.gz',FName),
  model3d(U,FName,A).
    
xml2agent(U,F,A):-
  fname2file('.xml.gz',F,FName),
  call_java_class_method('prolog3d.Convert',xml2shape(FName),Shape3D), % provided by prolog3d
  vertex3d(U,Shape3D,F,A),
  scale_to(A,0.1),
  auto(A,1).

xmls2morph(U,Names,A):-
  map(fname2file('.xml.gz'),Names,Files),
  morph3d(U,Files,morph,A),
  scale_to(A,0.1),
  auto(A,1).

fname2file(Suf,F,FName):- 
  model_dir(MD0),
  namecat(MD0,'XML','/',MD),
  namecat(MD,F,Suf,FName).

/**************** Prolog3D TESTS ****************/

boxtest:-
  set_bg(''),
  new_world(U),
  % box(U,B),
  box(U,1.0,0.0,0.0,0.2,B),
  show_world(U),
  sleep(1),
  inc_x(B,0.5),
  set_color(B,0.0,0.0,1.0),
  sleep(1),
  scale_to(B,2.0),
  sleep(1),
  auto(3),
  inc_rot_y(B,0.7),
  sleep(10),
  stop_world(U).
   
shapes3d:-simple3d,other3d.
  
simple3d:-
  set_bg('moon.jpg'),
  N=7,
  new_world(U),
  findall(A,(
    for(I,0,N),
      shaped_agent(U,I,'simple3d',A),
      Z is -(1+I),inc_z(A,Z),
      Y is (I-N/2)/3,inc_y(A,Y)
    ),As),
  show_world(U),
  sleep(5),
  foreach(member(A,As),auto(A,1)),
  sleep(10),
  foreach(member(A,As),stop_agent(A)),
  stop_world(U),
  set_bg('bg.jpg').

other3d:-
  N1=8,N2=11,Txt='Hello!',
  new_world(U),
  frozen_shape(U,Frost),
  frozen_shape(U,Frozen),
  mobile_edge3d(U,Frost,Frozen,"frozen",_E),
  findall(A,(
    for(I,N1,N2),
      shaped_agent(U,I,Txt,A)
    ),As),
  show_world(U),
  run_layout(U,400),
  auto(Frozen,1),
  sleep(10),
  foreach(for(I,1,20),(S is 0.5+0.1*I,scale_to(Frost,S),sleep_ms(200))),
  sleep(2),
  foreach(for(I,1,20),(S is 0.5+0.1*(20-I),scale_to(Frost,S),sleep_ms(200))),
  foreach(member(A,As),auto(A,1)),
  sleep(10),
  %println('HERE'),
  foreach(member(A,As),stop_agent(A)),
  stop_agent(Frost),
  stop_agent(Frozen),
  stop_world(U).
  
g31:-
  new_world(U),
  text3d(U,'Rotate me!',A),
  show_world(U),
  sleep(10),
  stop_agent(A),
  stop_world(U).
   
g32:-
   new_world(U),
   pyram(U,A),
   sphere(U,B),
   set_x(B,0.6),
   set_x(A,-0.6),
   show_world(U,240,320),
   sleep(10),
   stop_agent(A),
   stop_agent(B),
   stop_world(U).
   
g33:-
   new_world(U),
   color_cube(U,A),
   show_world(U),
   sleep(5),
   inc_z(A,-1),
   set_wake_up(A,500),
   (for(_I,1,10),
     inc_rot_z(A,0.1),
     inc_x(A,0.1),
     get_agent_state(A,S),
     println(state=S),
     % sleep(1),
     fail
   ;true
   ),
   (for(_I,1,10),
     inc_rot_z(A,0.1),
     inc_x(A,-0.1),
     get_agent_state(A,S),
     println(state=S),
     % sleep(1),
     fail
   ;true
   ),
   sleep(5),
   auto(A,true),
   sleep(10),
   stop_agent(A),
   stop_world(U).
   
    
gh:-     
   new_world(U),
   text3d(U,'Hiding!',A),
   inc_x(A,0.2),
   show_world(U),
   (for(_,1,3),
     sleep(2),
     println(hiding=A),
     hide_agent(A),
     sleep(2),
     println(showing=A),
     show_agent(A),
     fail
   ; true
   ),
   stop_agent(A),
   stop_world(U).

% graph layout demos

gr1:-
  println(begin_showing),     
  new_world(U),
  shaped_agent(U,0,hello,V1),move_to(V1,-0.5,0.5,-0.5),
  shaped_agent(U,0,bye,V2),move_to(V2,0.5,0,-0.5),
  edge3d(U,V1,V2,hello,_E),
  mobile_edge3d(U,V1,V2,bye,_ME),
  show_world(U),
  sleep(10),
  println(finished_showing),
  stop_world(U).
  
gr:-gr(15).

gr(N):-
  % set_interactive(0),
  AvgVertices=50,
  AvgOutgoingEdges=4,
  TrimLevel=0,
  % random_ranked_graph(AvgVertices,AvgOutgoingEdges,G,Info),
  new_cat(G),randomize_graph(G,AvgVertices,AvgOutgoingEdges,Info),
  % TG=G,
  trim_ranked_graph(G,TrimLevel,80,TG),
  println(Info),
  draw_graph(TG,N,400,400,400).

hgr:-
  new_cat(G),
  add_hyper_edge(G,ab,[a,b]),
  add_hyper_edge(G,bc,[b,c]),
  add_hyper_edge(G,ca,[c,a]),
  % dualize_hyper(G),
  % ranked_graph_info(G),
  draw_graph(G).
  
dhg:-
  new_cat(G),
  add_hyper_arrow(G,ab,[a,b],[c,d]),
  draw_graph(G).

hgr1:-
  new_cat(G),
  Lss=
   [[a, d, f],[a, d,g],[a, e, f],[a, e, g],
    [b, d, f],[b, d,g],[b, e, f],[b,e,g],
    [c, d, f],[c, d, g],[c, e, f],[c, e,g]],
  ( member(Ls,Lss),
    make_cmd(Ls,H),
    add_hyper_edge(G,H,Ls),
    fail
  ; true
  ),
  draw_graph(G).
  
posegr:-
  AvgVertices=100,
  AvgOutgoingEdges=10,
  % random_ranked_graph(AvgVertices,AvgOutgoingEdges,G,Info),trim_ranked_graph(G,2,100,TG),
  new_cat(G),
  randomize_graph(G,AvgVertices,AvgOutgoingEdges,Info),TG=G,
  println(Info),
  draw_graph(TG,100,400,400,400).
   
      
gr2:-
  new_world(U),
  gr2(U),
  show_world(U),
  sleep(15),
  stop_world(U).
          
gr2(U):-
  new_cat(G),randomize_graph(G,Info),
  % random_ranked_graph(G,Info),
  println(Info),
  findall(V-VA,(
       vertex_of(G,V),
       % vertex_data(G,V,VD),
       VD=V,
       shaped_agent(U,0,VD,VA) %,auto(VA,1)
     ),
     VAs),
   member(V-VA,VAs),
     outgoing_of(G,V,To),
       member(To-ToA,VAs),
       %edge_data(G,V,To,ED),
       ED=e(from=V,to=To),
         edge3d(U,VA,ToA,ED,_EA),
  fail.
gr2(U):-run_layout(U,400).          
       

smalltest:-call_java_class_method('Test',smalltest,_).

rantest:-call_java_class_method('Test',rantest,_).

refs3d:-refs3d(prolog3d,20).

jrefs:-add_to_path('/tarau/prolog/psrc/'),refs3d(prolog,0).

refs3d(File):-refs3d(File,20).

refs3d(File,Time):-
  set_format_precision(2),
  xref(File,G,Info),
  println(Info),
  show_graph(G),
  draw_graph(G,Time,400,400,300).

modeltest:-
  one_modeltest,
  bodytest,
  morphtest,
  all_modeltest,
  jftest,
  xmodeltest.


% animates an agent based on a j3f model
jftest:-
  new_world(U),
  add_controls(U),
  %jf2agent(U,woman,A),
  model_file(woman,WoManFile),
  body3d(U,WoManFile,woman,A),
  scale_to(A,3.0),
  set_y(A,-0.4),set_x(A,-0.4),set_z(A,-0.5),
  model_file(moon,MoonFile),
  satellite(U,A,14,1.1,MoonFile,Moon),
  scale_to(Moon,0.05),
  set_y(Moon,0.4),
  % set_holder(U,Moon),
  model_file(man,ManFile),
  body3d(U,ManFile,man,E),
  set_x(E,0.4),set_y(E,-0.8),set_z(E,-1.0),inc_rot_y(E,-0.8),
  % edge3d(U,E,A,hello,_),
  % mobile_edge3d(U,E,A,bye,_ME),
  show_world(U),
  N=100,
  (for(I,1,N),
    ( S is 1-I/N,
      scale_to(E,S),
      inc_rot_y(A,0.2),
      sleep_ms(50),
      fail
    )
  ; true  
  ),
  (for(I,1,N),
    ( S is I/N,
      scale_to(E,S),
      sleep_ms(50),
      fail
    )
  ; true  
  ),
  auto(E,1),
  auto(A,1),
  sleep(15),
  stop_world(U).
  

% animates an agent based on a j3f model
pose1:-
  new_world(U),
  add_controls(U),
  centered_man(U,M),
  inc_x(M,0.5),
  jf2agent(U,moon,Moon),
  scale_to(Moon,0.10),
  set_y(Moon,0.6),set_z(Moon,-1.2),set_x(Moon,0.0),inc_rot_y(Moon,-1.8),inc_rot_x(Moon,0.5),
  jf2agent(U,girl,W),
  set_y(W,-0.8),set_x(W,-0.8),set_z(W,-1.0),inc_rot_y(W,1.8),
  % auto(W,1),
  %model_file(man,ManFile),
  %body3d(U,ManFile,man,E),
  %set_y(E,-0.8),set_x(E,0.8),set_z(E,-1.0),
  show_world(U),
  join_hands(M),
  inc_rot_y(M,-0.8),
  sleep(1),
  head_says_yes(M),
  (for(_I,1,10),
    inc_z(Moon,0.04),
    inc_x(Moon,-0.02),
    sleep_ms(50),
    fail
  ; true
  ),
  sleep(20),
  auto(Moon,1),
  auto(M,1),
  sleep(20),
  stop_world(U).
    
% animates an agent based on a VRML model
vrmltest:-
  new_world(U),
  foreach(
    nth_member(F,[
     robot,
     girl,
     soldier,
     joe
    ],N),
    ( vrml2agent(U,F,A),
      scale_to(A,0.8),
      set_y(A,-1.0),
      X is -1.5+N/2,set_x(A,X),
      Z is -1.5+N/2,set_z(A,Z)
    )
  ), 
  show_world(U),
  sleep(20),
  stop_world(U).
  
% animates an agent based on a Wavefront OBJ model
one_modeltest:-
  new_world(U),
    jf2agent(U,moon,Agent),
    inc_z(Agent,-25),
  show_world(U),
  foreach(
    for(_I,1,200),
    ( inc_z(Agent,0.1),
      inc_x(Agent,0.001),
      inc_rot_y(Agent,0.1),
      sleep_ms(20)
    )
  ),
  sleep(10),
  stop_world(U).

art:-
 R=400,
 new_world(U),
 sphere(U,Root), % auto(Root,3),
  sphere(U,Sink),auto(Sink,3),
  model_dir(D),
  ( dir_has_file(D,F),
    trim_suffix(F,_Name,Suf),
    Suf=='.j3f.gz',
    random(X),0=:=X mod 2,
    namecat(D,F,'',File),
    model3d(U,File,A),scale_to(A,0.05),
    edge3d(U,Root,A,edge,_E),
    random(Y),0=:=Y mod 4,
    edge3d(U,A,Sink,edge,_ME),
    fail
  ; true
  ),
  add_controls(U),
  show_world(U,R,R),
  set_view(U,0,1,10),
  run_layout(U,R),
  sleep(10),
  set_z(Sink,-10),
  set_holder(U,Sink),
  sleep(30),
  stop_world(U).
    
% animates a few VRML, Wavefront OBJ and Blender XML models    
all_modeltest:-
  new_world(U),
  sphere(U,Root),auto(Root,1),
  sphere(U,Sink),auto(Sink,1),
  mobile_edge3d(U,Sink,Root,0,_),
  foreach(
    ( model_files(_,Fs),
      nth_member(F,Fs,N)
    ),
    ( 
      println(loading(F)),
      jf2agent(U,F,A),
      println(loaded(F)),
      edge3d(U,Root,A,N,_E)
      ,edge3d(U,A,Sink,N,_ME)
    )
  ),
  add_controls(U),
  R=400,
  show_world(U,R,R),
  run_layout(U,R),
  sleep(10),
  set_holder(U,Root),
  sleep(5),
  set_holder(U,Sink),sleep(5),
  remove_holder(U),
  ( for(I,1,100),
     ZD is I * 0.05,
     set_view(U,0,0,ZD),
     sleep_ms(100),
     fail
   ; true
  ),
  auto(Root,0),   
  auto(Sink,0),
  sleep(30),
  stop_world(U).


xmodeltest:-  
  new_world(U),
  foreach(
    member(F,[
      'BrownHorse',
      'BlueScorpion'
    ]),
    xml2agent(U,F,_A)
  ),
  add_controls(U),
  show_world(U),
  sleep(20),
  stop_world(U).

bodytest:-
  set_bg('hubble.jpg'),
  new_world(U),
  add_controls(U),
  model_file(man,ManFile),
  body3d(U,ManFile,'man',B),
  set_y(B,-0.8),
  set_z(B,-0.8),
  show_world(U),
  sleep(5),
  auto(B,1),
  sleep(20),
  set_bg('bg.jpg'),
  stop_world(U).
  
% animates Blender models using morph nodes  
morphtest:-
  new_world(U),
  Horses=['BrownHorse01','BrownHorse02','BrownHorse03','BrownHorse04'],
  xmls2morph(U,Horses,BH1),
    set_x(BH1,-0.5),set_y(BH1,-0.5),inc_rot_y(BH1,1.1),
  xmls2morph(U,Horses,BH2),
    scale_to(BH2,0.05),set_x(BH2,0.5),set_y(BH2,-0.5),inc_rot_y(BH2,1.1),
  xmls2morph(U,Horses,BH3),inc_rot_x(BH3,-0.5),set_z(BH3,-10.0),
    set_y(BH3,-0.5),inc_rot_y(BH3,3.50),
  tetra(U,A),auto(A,1),set_y(A,0.5),
  add_controls(U),
  show_world(U),
  bg(foreach(for(_,1,300),(inc_rot_y(BH1,0.03),sleep_ms(50)))),
  bg(foreach(for(_,1,300),(inc_rot_y(BH2,-0.03),sleep_ms(50)))),
  bg(foreach(for(_,1,300),(inc_z(BH3,0.03),sleep_ms(50)))),
  sleep(20),
  stop_world(U).
  
file2jf(F,JF):-call_java_class_method('prolog3d.Convert',file2jf(F,JF),_). 

model_files(vrml,
        [ soldier,egghead,joe,moon,nancy,sarah,
          witch,skull,sphinx,spook,witch,
          girl,sphinx
        ]).
model_files(obj,
       [ beethoven,submarine,rocket,galleon,
         hellskull,octopus,roman,unicycle,
         helicopter
       ]).
model_files(body,
        [man,woman]). % cyborg,robot]).

convert_subdir(FromSubDir):-convert_subdir(FromSubDir,'.').

convert_subdir(FromSubDir,ToSubDir):-
   model_dir(D),
   namecat(D,FromSubDir,'/',FromPath),
   namecat(D,ToSubDir,'/',ToPath),
   convert_dir(FromPath,ToPath).
   
convert_dir(AbsFromPath,AbsToPath):-
   dir_has_file(AbsFromPath,File),
   trim_suffix(File,FName,Suf),
   \+(member(Suf,['.jpg','.JPG','.gif','.GIF','.png','.PNG','.txt','.mtl','html'])),
   namecat(AbsFromPath,File,'',F),
   namecat(AbsToPath,FName,'.j3f.gz',JF),
   (Suf=='.wrl'->Op=body3d;Op=model3d),
   show_conversion(Op,F,JF),
   fail
; true.   
    
trim_suffix(FS,F,Suf):-
  "."=[Dot],
  atom_codes(FS,Cs),
  append(Ns,[Dot|Ss],Cs),
  !,
  atom_codes(F,Ns),
  atom_codes(Suf,[Dot|Ss]).

convert:-
  convert_vrml,
  convert_obj,
  convert_body.

convert_vrml:-convert_subdir('VRML').
convert_obj:-convert_subdir('OBJ').
convert_body:-convert_subdir('BODIES').
convert_xml:-convert_subdir('XML').

convert_test:-convert_subdir('TEST').
  
show_conversion(Op,F,JF):-
  println(converting(F=>JF)),
  file2jf(F,JF),
  show_model_abs(Op,JF).

show_model(Op,F):-
  model_file(F,AbsJF),
  show_model_abs(Op,AbsJF).

show_model_abs(Op,JF):-
  new_world(U),
  call(Op,U,JF,A),
  show_world(U),
  sleep(5),
  auto(A,1), 
  (Op==body3d->sleep(15);sleep(5)),
  stop_world(U).

show_body(F):-
  model_file(F,AbsJF),
  show_body_abs(AbsJF).
      
show_body_abs(JF):-
  new_world(U),
  body3d(U,JF,'body',A),
  inc_y(A,-0.9),
  inc_z(A,-0.5),
  show_world(U),
  object_to_string(A,S),println(S),
  sleep(5),
  auto(A,1), 
  sleep(20),
  stop_world(U).

model_file(F,AbsF):-
  model_file(F,'.j3f.gz',AbsF).

model_file(F,S,AbsF):-
  model_dir(D),
  namecat(D,F,S,AbsF).
  
model_dir(D):-is_applet,!,model_dir1(D).
model_dir(D):-model_dir1(D0),is_directory('.',D0),!,D=D0.  
model_dir(D):-model_dir2(D0),is_directory('/',D0),!,D=D0.  
model_dir(D):-model_dir3(D).

model_dir1('models/').
model_dir2('C:/paul/models/').
model_dir3('http://localhost:8001/').

/***********  anthropomorphic agent manipulation **********/

% generic elementary movements

walk(A):-
  invoke_java_method(A,walk,_).

set_turn(A,Degrees):- % default 30 degrees
  invoke_java_method(A,setTurn(Degrees),_).
  
set_step(A,Units):- % default 0.1 units
  invoke_java_method(A,setStep(Units),_).
  
turn(left,A):-
  invoke_java_method(A,turn_left,_).
turn(right,A):-
  invoke_java_method(A,turn_right,_).
turn(up,A):-
  invoke_java_method(A,turn_up,_).
turn(down,A):-
  invoke_java_method(A,turn_down,_).
turn(back,A):-
  invoke_java_method(A,turn_back,_).
    
tilt(left,A):-
  invoke_java_method(A,tilt_left,_).
tilt(right,A):-
  invoke_java_method(A,tilt_right,_).

hurry(A):-
  invoke_java_method(A,hurry,_).

relax(A):-
  invoke_java_method(A,relax,_).

% gesture components

lrrl(A):-
  turn(left,A),
  turn(right,A),
  turn(right,A),
  turn(left,A).

uddu(A):-
  turn(up,A),
  turn(down,A),
  turn(down,A),
  turn(up,A).


% gestures
        
head_says_no(A):-
  body_set_focus(A,'HEAD'),
  lrrl(A),
  body_reset_focus(A).

head_says_yes(A):-
  body_set_focus(A,'HEAD'),
  uddu(A),
  body_reset_focus(A).

join_hands(A):-
  join_hand(A,'SH_L','ELBOW_L'),
  ndo(3,tilt(left,A)),
  join_hand(A,'SH_R','ELBOW_R'),
  ndo(3,tilt(right,A)),
  body_reset_focus(A).
  
join_hand(A,Sh,El):-
  body_set_focus(A,Sh),
  ndo(3,turn(up,A)),
  body_set_focus(A,El),
  ndo(3,turn(up,A)).

turn_legs(A):-
  turn_legL(A,'THIGH_L'),
  %ndo(3,tilt(left,A)),
  turn_legR(A,'THIGH_R'),
  %ndo(3,tilt(right,A)),
  body_reset_focus(A).
  
turn_legL(A,Sh):-
  body_set_focus(A,Sh),
  ndo(3,turn(left,A)),
  body_set_focus(A,Sh),
  ndo(3,turn(right,A)).

turn_legR(A,Sh):-
  body_set_focus(A,Sh),
  ndo(3,turn(right,A)),
  body_set_focus(A,Sh),
  ndo(3,turn(left,A)).

turn_stomach(A):-
  body_set_focus(A,'STOMACH'),
  ndo(5,turn(left,A)),
  body_set_focus(A,'STOMACH'),
  ndo(5,turn(right,A)).

turn_chest(A):-
  body_set_focus(A,'CHEST'),
  ndo(5,turn(right,A)),
  body_set_focus(A,'CHEST'),
  ndo(5,turn(left,A)).

raise_lhand(A):-
  body_set_focus(A,'SH_L'),
  ndo(3,turn(up,A)),
  body_set_focus(A,'SH_L'),
  ndo(3,turn(down,A)).

raise_lfoot(A):-
  body_set_focus(A,'FOOT_L'),
  ndo(3,turn(up,A)),
  body_set_focus(A,'FOOT_L'),
  ndo(3,turn(down,A)).
 
raise_lleg(A):-
  body_set_focus(A,'THIGH_L'),
  ndo(3,turn(up,A)),
  body_set_focus(A,'THIGH_L'),
  ndo(3,turn(down,A)).

shake_head(A):-
  body_set_focus(A,'HEAD'),
  ndo(1,turn(down,A)),
  body_set_focus(A,'HEAD'),
  ndo(1,turn(up,A)).
   
% tools

ndo(N,G):-
  for(_I,1,N),
  G,
  fail.
ndo(_N,_G).

% tests

ttest:-
  new_world(U),
  centered_man(U,A),
  show_world(U),
  join_hands(A),
  body_set_focus(A,'CHEST'),
  body_set_focus(A,'PELVIS'),
  turn(left,A),turn(right,A),
  tilt(left,A),sleep(5),tilt(right,A),sleep(1),tilt(right,A),sleep(1),tilt(left,A),
  turn(up,A),sleep(1),turn(down,A),turn(down,A),sleep(1),turn(up,A),
  sleep(5),
  stop_world(U).

rtest:-
  new_world(U),
  add_controls(U),
  centered_man(U,A),
  %jf2agent(U,woman,A),set_y(A,-1),set_z(A,-1),
  set_turn(A,60),
  show_world(U),
  Gs=[
      turn(left,A),turn(right,A),
      tilt(left,A),tilt(right,A),tilt(left,A),tilt(right,A),
      turn(left,A),turn(up,A),
      %walk(A),walk(A),walk(A)
      turn(down,A),turn(up,A),turn(down,A),turn(right,A)
  ],
  map(rtest1,Gs),
  stop_world(U).

rtest1(G):-
  println(action=G),
  sleep(2),
  G,
  sleep(3).
      
centered_man(U,A):-
  model_file(man,ManFile),
  body3d(U,ManFile,man,A),
  object_to_string(A,S),println(S),
  set_y(A,-1.0),
  set_z(A,-2.0).
  
 btest:-
  new_world(U),
  centered_man(U,A),
  show_world(U),
  head_says_yes(A),
  sleep(1),
  turn(back,A),
  ndo(2,(walk(A),turn(left,A))),
  turn(back,A),
  ndo(2,(turn(right,A),walk(A))),
  head_says_no(A),
  sleep(5),
  auto(A,1),
  sleep(3),
  auto(A,0),
  sleep(2),
  stop_world(U).

action-->[turn].
action-->[advance].
action-->action,[and],action.

eval([],_).
eval([Action|As],Agent):-call(Action,Agent),eval(As,Agent).

turn(A):-println(turning=A),inc_rot_y(A,0.785),sleep_ms(100).
advance(A):-println(advancing=A),inc_x(A,0.2),sleep_ms(100).
and(_).

do(Action):-
  new_world(U),
  box(U,Agent),
  show_world(U),
  sleep(3),
  action(Action,[]),
  eval(Action,Agent),
  sleep(5),
  stop_world(U).

atest:-
  do([
    turn,and,advance,and,turn,and,advance
  ]).



cboxtest:-
  Max is 100,
  set_bg(''),
  new_world(U),
  box(U,1.0,0.0,0.0,0.2,B),
  show_world(U),
  sleep(5),
  (for(_,1,Max),
     inc_z(B,-0.05),
     sleep_ms(100),
     fail
   ; true
  ),
 (for(_,1,Max),
     inc_z(B,0.05),
     sleep_ms(100),
     fail
   ; true
  ),
  set_color(B,0.0,0.0,1.0),
 (for(_I,1,Max),
     sleep_ms(100),
     inc_blue(B,-0.01),
     inc_green(B,0.01),
     % Blue is 1.0/I,set_color(B,0.0,1.0,Blue),
     fail
   ; true
  ),

  sleep(5),
  scale_to(B,2.0),
  sleep(3),
  inc_rot_y(B,0.7),
  sleep(5),
  stop_world(U).
 
tetvrml:-
  % set_interactive(0),
  new_world(W),
  jf2agent(W,tet,A),
  add_controls(W),
  show_world(W),
  auto(A,1),
  sleep(5).

tet:-
  new_world(U),
  box(U,B),move_to(B,0,-1,0),scale_to(B,20,0.2,20),set_color(B,1,1,0),
  sphere(U,V1),move_to(V1,0,1,1),set_color(V1,1,0,0),
  sphere(U,V2),move_to(V2,1,0,1),set_color(V2,0,1,0),
  sphere(U,V3),move_to(V3,1,1,0),set_color(V3,0,0,1),
  sphere(U,V4),move_to(V4,0,0,0),set_color(V4,1,1,1),
  edge3d(U,V1,V2,"12",_),
  edge3d(U,V1,V3,"13",_),
  edge3d(U,V1,V4,"14",_),
  edge3d(U,V2,V3,"23",_),
  edge3d(U,V2,V4,"24",_),
  edge3d(U,V3,V4,"34",_),
  set_view(U,0,2,10),
  show_world(U),
  sleep(5),
  auto(V4,1),
  sleep(20),
  stop_world(U).
 
 
trimtest:-
  call_java_class_method('Test',trimtest,OStack),
  println(OStack).
    
    
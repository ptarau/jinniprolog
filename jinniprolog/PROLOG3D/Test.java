import prolog3d.*;
import java.io.*;

import javax.media.j3d.*;
import javax.vecmath.*;
import prolog.core.Cat;
import prolog.logic.*;
import prolog.kernel.*;

/**
   Various test methods
 */
public class Test {
  
  public static void main(String[] args) {
    //mtest();
    //vrmltest();
    //htest();
    //wtest();
    //ttest();
    //rantest();
    //trimtest();
    treetest();
    //colortest();
    //spintest();
    //smalltest();
    //tet();
    //morphtest();
    //ctest();
    //Animal.zootest();
    //dhyp();
    //hyp();
  }

   public static void mtest() {
    Prolog3D M=new Prolog3D();
    World world=M.getWorld();
    Model 
       aModel=new Model(world,
         new String[]{"models/rocket.j3f.gz","/paul/models/rocket.j3f.gz"},
      "model"
    );
    M.addControls();
    M.showWorld(400,400);
    aModel.animate();
    Params.sleep(20000);
    M.stopWorld();
    M.exit();
  }

  public static void vrmltest() {
    Prolog3D M=new Prolog3D();
    World world=M.getWorld();
    Body 
      aModel=new Body(world,
      new String[]{"C:/paul/models/TEST/witch.wrl"},
      "witch"
      );
    M.addControls();
    M.showWorld(400,400);
    aModel.setAuto(1);
    Params.sleep(20000);
    M.stopWorld();
    M.exit();
  }

  /**
     Tests some Humanoid characters
   */
  public static void htest()  {
    Prolog3D M=new Prolog3D();
    World W=M.getWorld();
    Man aMan=new Man(W);Prolog3D.pp("model="+aMan);
    Woman aWoman=new Woman(W); Prolog3D.pp("model="+aWoman);
    Human aCyborg=new Human(W,"cyborg");  Prolog3D.pp("model="+aCyborg);
    Human aRobot=new Human(W,"robot");  Prolog3D.pp("model="+aRobot);
    M.addControls();
    M.showWorld(600,600);
    aMan.setX(0.5);
    aCyborg.scaleTo(0.2); aCyborg.setX(1.2);
    aRobot.scaleTo(0.05);aRobot.setX(-1.2);
    M.setView(0,1,5);
    aMan.animate();
    aWoman.animate();
    //aCyborg.animate();
    //aRobot.animate();
    Params.sleep(10000);
    //aWoman.setAuto(1);
    Params.sleep(10000);
    M.stopWorld();
    M.exit();
  }

  /**
   Tests walking, turning tilting etc.
   */
  public static void wtest()  {
    Prolog3D M=new Prolog3D();
    World W=M.getWorld();
    //Vertex3D A=new Vertex3D(W,Shape.CCUBE,1.0,0.0,0.0,0.2,"Cube");
    Vertex3D A=new Woman(W);
    //Vertex3D A=new Man(W);
    M.addControls();
    M.showWorld(500,500); 
    A.center();
    Prolog3D.pp("agent:"+A);
    //A.setTurn(60);
    //M.setView(0,0,2); 
    /*for(int i=0;i<16;i++) {
      A.turn_right();
      Params.sleep(100);
    }*/ 
    //A.turn_left();
    Params.sleep(1000);
    A.turn_left(); A.turn_left(); A.turn_left();
    A.turn_up();
    A.walk();
    A.walk();
    Params.sleep(1000);
    //A.turn_up();
    A.walk();
    A.walk();
  
    Params.sleep(30000);
    M.stopWorld();
    M.exit();
  }

  /**
   Tests walking, turning tilting etc.
   */
  public static void ttest()  {
    Prolog3D M=new Prolog3D();
    World W=M.getWorld();
    Vertex3D A=null;
    int choice=Params.ri(4);
    choice=3;
    switch(choice) {
      case(0):A=new Vertex3D(W,Shape.CCUBE,1.0,0.0,0.0,0.2,"Cube");A.scaleTo(1,2,0.5);break;
      case(1):A=new Woman(W);break;
      case(2):A=new Man(W);break;
      case(3):A=new Octo(W);break;
    }
    M.addControls();
    M.showWorld(600,600); 
    A.center();
    Prolog3D.pp("agent:"+A);
    int n=3;
    for(int i=0;i<n;i++){Prolog3D.pp("turn_left:");Params.sleep(1000);A.turn_left();}
    for(int i=0;i<n;i++){Prolog3D.pp("turn_right:");Params.sleep(1000);A.turn_right();}
    for(int i=0;i<n;i++){Prolog3D.pp("turn_up:");Params.sleep(1000);A.turn_up();}
    for(int i=0;i<n;i++){Prolog3D.pp("turn_down:");Params.sleep(1000);A.turn_down();}
    for(int i=0;i<n;i++){Prolog3D.pp("tilt_left:");Params.sleep(1000);A.tilt_left();}
    for(int i=0;i<n;i++){Prolog3D.pp("tilt_right:");Params.sleep(1000);A.tilt_right();}
    M.setView(0,0,12); 
    for(int i=0;i<n;i++){
       Prolog3D.pp("walk left right:");Params.sleep(1000);
       A.turn_left();A.walk();A.turn_right();A.turn_right();A.walk();A.turn_left();
    }
    A.center();A.turn_left();
    for(int i=0;i<n;i++){
       Prolog3D.pp("walk up down:");Params.sleep(1000);
       A.turn_up();A.walk();A.turn_down();A.turn_down();A.walk();A.turn_up();
    }
    A.turn_right();
    A.setAuto(1);
    Params.sleep(10000);
    M.stopWorld();
    M.exit();
  }
  
  /**
     Generates a random graph depending on given parameters
     for testing various algorithms.
   */
  public static Cat ranGraph() {
    int seed=0;
    int v0=50;
    int v=10;
    int e0=5;
    int e=10;
    int g=0;
    int m=15;
    Cat RG=
      Params.randomRanked(seed,v0,v,e0,e,g,m);
    return RG;
  }
 
 

  /**
    Tests layout algorithm on a random graph.
   */
  public static void rantest() {   
    int time=10;  
    int r=400;
    int w=400;
    int h=400;
    Prolog3D.drawGraph(ranGraph(),time,r,w,h);
  }


 /**
    Tests trimming algorithm on a random graph.
   */
  public static ObjectStack trimtest() {   
    int time=10;  
    int r=400;
    int w=400;
    int h=400;
    
    int seed=0;
    int v0=50;
    int v=30;
    int e0=2;
    int e=4;
    
    Cat RG=Params.randomCat(seed,v0,v,e0,e);
    ObjectStack S=new ObjectStack();
    do {  
      RG.runGraphRanker();RG.markComponents();RG.rankSort();
      int cycles=RG.checkCycle();
      
      S.push(RG);

      Prolog3D.pp("\nas saved:"+RG);
      Prolog3D.pp("cycles="+cycles+",size="+RG.size());
    

      Prolog3D.drawGraph(RG,time,r,w,h);
      
      RG=(Cat)RG.trim(false,Math.round(RG.size()/2));

     
    } 
    while(RG.size()>0);
    //Prolog3D.pp("S="+S);
    return S;
  }

  public static void treetest() {   
    int r=400;
    int w=400;
    int h=400;
    
    int seed=0;
    int v0=20;
    int v=10;
    int e0=4;
    int e=6;
    
    Cat RG=Params.randomCat(seed,v0,v,e0,e);

    Prolog3D.pp("\nas saved:"+RG);
    //Prolog3D.pp("cycles="+cycles+",size="+RG.size());
    
    Prolog3D M=new Prolog3D("TreeTest");

    Prolog3D.drawTree(M,r,RG);

    M.showWorld(w,h);
  }


  /**
   Tests layout algorithm on a random graph.
   */
  public static void colortest() {   
    int time=30;  
    int r=400;
    int w=400;
    int h=400;
    Params.bgfile="";
    Prolog3D U=new Prolog3D();
    Vertex3D V=new Vertex3D(U.getWorld(),4,0,0,1,0,"colors");
    U.showWorld(w,h);
    V.setAuto(2);
    Params.sleep(time*1000);
    U.stopWorld();
  }

  /**
     tests satellites - agents depending on other agents' positions
   */
  public static void spintest() {
    Prolog3D M=new Prolog3D(); 
    Simple sun=new Sphere3D();sun.transformScale(0.1);
    Vertex3D Sun=M.addVertex(sun,"Sun");
    Simple earth=new Cube(new Color3f(1,1,1),0f);earth.transformScale(0.05);
    Satellite Earth=new Satellite(M.getWorld(),Sun,earth,0.8,"Earth");

    Simple moon=new Pyram();moon.transformScale(0.5);
    Satellite Moon=new Satellite(M.getWorld(),Earth,moon,0.4,"Moon");

    Simple sat=new CCube();
    sat.transformScale(0.1);
    Satellite Apollo=new Satellite(M.getWorld(),Moon,sat,0.2,"Apollo");
    M.showWorld(400,400);
    
    for(int i=0; i<200;i++) {
      //Transform3D T=new Transform3D();
      //T.setEuler(new Vector3d(0.1,0,0));
      //Sun.getGroup().setTransform(T);
      Sun.setZ(1-i/50.0);
      Params.sleep(200);
    }
    
    Params.sleep(2000);
    M.stopWorld();
  }
  
  /**
     tests building and drawing a simple graph
   */
  public static void smalltest() {
    Prolog3D M=new Prolog3D();
    
    Vertex3D A=M.addVertex(7,null,null);A.scaleTo(0.5);A.setAuto(1);  
    Vertex3D V1=M.addVertex("one");
    Vertex3D V2=M.addVertex("two");V2.setAuto(1);
      
    Vertex3D V3=M.addVertex("three");
    Vertex3D V4=M.addVertex("four");
    float k=1.0f;
    
    V1.moveTo(rf(k),rf(k),rf(k)-1);
    V2.moveTo(rf(k),rf(k),rf(k)-1);
    V3.moveTo(rf(k),rf(k),rf(k)-1);
    V4.moveTo(rf(k),rf(k),rf(k)-1);
    
    Edge3D E=M.addEdge(V2,A,"E");
    Edge3D E12=M.addEdge(V1,V2,"12");
    
    Edge3D E23=M.addEdge(V2,V3,"23");
    Edge3D E34=M.addEdge(V3,V4,"34");
    Edge3D E41=M.addEdge(V4,V1,"41");
    MobileEdge3D ME12=M.addMobileEdge(V1,V2,"12");
    MobileEdge3D ME23=M.addMobileEdge(V2,V3,"23");
    MobileEdge3D ME34=M.addMobileEdge(V3,V4,"34");
    MobileEdge3D ME41=M.addMobileEdge(V4,V1,"41");
   
    M.showWorld(400,400);
    M.runLayout(400);
    Params.sleep(10000);
    V1.setAuto(1);
    Params.sleep(5000);
    M.stopWorld();
  }

  public static void dhyp() {
    Params.bgfile="";
    Params.bgColor=new Color3f(0,0,0);
    Cat G=new Cat();
   
    G.addHyperArrow("ABC->DE",new Object[]{"A","B","C"},new Object[]{"D","E"});
    G.addHyperArrow("BD->EFG",new Object[]{"B","D"},new Object[]{"E","F","G"});
    G.addHyperArrow("CF->B",new Object[]{"C","F"},new Object[]{"B"});
    G.addHyperArrow("BG->DF",new Object[]{"B","G"},new Object[]{"D","F"});
    G.addHyperArrow("FG->ABCD",new Object[]{"F","G"},new Object[]{"A","B","C","D"});
    //G.dualize();
    Prolog3D.pp(Cat.showInfo(G));
     
    int r=400;
    int w=600;
    int h=600;
    Prolog3D.drawGraph(G,0,r,w,h);
  }
  
   public static void hyp0() {
    Params.bgfile="";
    Params.bgColor=new Color3f(0,0,0);
    Cat G=new Cat();
   
    G.addHyperEdge("ABC",new Object[]{"A","B","C"});
    G.addHyperEdge("BD",new Object[]{"B","D"});
    G.addHyperEdge("CD",new Object[]{"C","D"});
    G.addHyperEdge("BCD",new Object[]{"B","C","D"});
    
    //G.dualize();
    Prolog3D.pp(Cat.showInfo(G));
     
    int r=400;
    int w=600;
    int h=600;
    Prolog3D.drawGraph(G,0,r,w,h);
  }
  
  public static void hyp() {
    Params.bgfile="";
    Params.bgColor=new Color3f(0,0,0);
    Cat G=new Cat();
   
    G.addHyperEdge("AB",new Object[]{"A","B"});
    G.addHyperEdge("BC",new Object[]{"B","C"});
    G.addHyperEdge("CA",new Object[]{"C","A"});
    
    //G.dualize();
    Prolog3D.pp(Cat.showInfo(G));
     
    int r=400;
    int w=600;
    int h=600;
    Prolog3D.drawGraph(G,0,r,w,h);
  }

  public static void tet() {
    Prolog3D M=new Prolog3D();
    // 000 011 101 110
    double l=0.5;
    double s=0.25;
    Vertex3D V1=M.addVertex("1");V1.scaleTo(s);V1.moveTo(0,0,0);V1.setColor(1,1,1);
    Vertex3D V2=M.addVertex("2");V2.scaleTo(s);V2.moveTo(0,l,l);V2.setColor(1,0,0); 
    Vertex3D V3=M.addVertex("3");V3.scaleTo(s);V3.moveTo(l,0,l);V3.setColor(0,1,0); 
    Vertex3D V4=M.addVertex("4");V4.scaleTo(s);V4.moveTo(l,l,0);V3.setColor(0,0,1); 

    Edge3D E12=M.addEdge(V1,V2,"12");
    Edge3D E13=M.addEdge(V1,V3,"13");
    Edge3D E14=M.addEdge(V1,V4,"14");
    Edge3D E23=M.addEdge(V2,V3,"23");
    Edge3D E24=M.addEdge(V2,V4,"24");
    Edge3D E34=M.addEdge(V3,V4,"34");
    //MobileEdge3D ME12=M.addMobileEdge(V1,V2,"12");
    M.addControls();
    M.showWorld(400,400);
    for(int i=0;i<5;i++) {M.setView(0,1,i+2);Params.sleep(1000);}
    Params.sleep(10000);
    V1.setAuto(3);
    V2.setAuto(3);
    V3.setAuto(3);
    V4.setAuto(3);
    Params.sleep(10000);
    V1.setAuto(2);
    V2.setAuto(2);
    V3.setAuto(2);
    V4.setAuto(2);
    //M.runLayout(400);
    Params.sleep(10000);
    M.stopWorld();
    M.exit();
  }

  /**
     Tests morphing
   */
  public static void morphtest() {
    Prolog3D M=new Prolog3D();
    Shape3D from=new PyramShape(new Color3f(1.0f,0.0f,0.5f));
    Shape3D to=new PyramShape(new Color3f(1.0f,1.0f,0.0f));
    Shape3D[] shapes={from,to};
    Morph3D A=M.addMorph(shapes,"Murphy");
    M.showWorld(400,400);
    A.setAuto(1);
    Params.sleep(10000);
    M.stopWorld();
  }
  
  /**   Tests drawing an attributed graph/category
   */
  public static void ctest() {
    Cat C=new Cat();
    C.setProp("one","p","1");
    C.setProp("two","p","2");
    C.setProp("two","pp","22");
    float k=1.0f;
    C.setMorphism("one","two","dir","12");
    C.setMorphism("two","one","dir","21");
    C.setMorphism("one","two","color","blue");
    Prolog3D.pp(Cat.showInfo(C));
    Prolog3D.drawGraph(C,10,400,400,400);
  }
  
  /**
     Generates random number from -f to f
   */
  public static float rf(float f) {
    return Params.rf(f);
  }
}

  /**
   Implements H-Anim-like human with a simple set of correct rotations
   good for humanoid animation. The key property is that they are pure 
   rotations and their moving parts are centered into the local coordinates.
   This ensures that rotation can safely applied to than and they will move as
   expected. If this is not the case (as it is with most models you find on the net,
   various body parts will start leaving their natural locations when rotated.
   */


class Human extends Body implements Runnable {
  public Human(World world,String[] FileOrURLs,Object data) {
    super(world,FileOrURLs,data);
  }
  
  public Human(World world,String name) {
    this(world,new String[]{mdir1+name+".j3f.gz",mdir2+name+".j3f.gz"},name);
  }

  public static String mdir1="models/";
  public static String mdir2="/paul/models/";

  public void center() {
    setFocus(null);
    setY(-1.0);
    setZ(-2.0);
    sleep();
  }
  
  /**
   ensure that sleep time is larger than the renderer's visits!
   */  
  public void walk() {
    //Prolog3D.pp(this.data+"=>walking, focus="+getFocus());
    //if(!isAlive()) 
    //   super.walk();
    //else 
    {
      step_with("SHANK_L","THIGH_L");
      step_with("SHANK_R","THIGH_R");
    }
  }
   
  /**
   makes a few steps - but needs more work to look real :-)
   */ 
  private void step_with(String s,String t) {
    if(setFocus(t)) {incRotX(-0.4);sleep();}
    super.walk();
    if(setFocus(s)) {incRotX(+0.6);sleep();}
    super.walk();
    if(setFocus(t)) {incRotX(+0.4);sleep();}
    super.walk();
    if(setFocus(s)) {incRotX(-0.6);sleep();}
    super.walk();
  }

  public void run() {
    act();
  }
  
  public void act() {
    for(int i=0;i<4;i++) {
      walk();
      for(int j=0;j<3;j++) {
        turn_right();
      }
    }
  }

  public void animate() {
    Thread T=new Thread(this,"Prolog3DThread");
    T.setDaemon(true);
    T.start();
  }
}

/**
   Simple girl model, unfortunately not easy to animate as joints are not named.
*/
class Girl extends Human {
  public Girl(World world) {
    super(world,"girl");
    clearTransforms();
    setFocus(null); 
  }
  
  public void run() {
    //center();
    //setX(-1.0);
    //incRotY(1.2);
    sleep();
    
    act();
    
    //Prolog3D.pp("here!! "+this.data);
  }
}

  /**
   Easy to control Human model. Simply apply rotations (mostly upon x and y axis)
   to the following joints (where _L and _R indicate left and right:

   THIGH_L,THIGH_R
   SHANK_L,SHANK_R,
   FOOT_L,FOOT_R,
   HEEL_L,HEEL_R,
   SH_L,SH_R (shoulders),
   ELBOW_L,ELBOW_R,
   HAND_L,HAND_R,
   PELVIS,STOMACH,CHEST,
   NECK,HEAD
 
   The model has the joints centered shuch that rotations will
   result in natural gestures.
   */
class Man extends Human {
  public Man(World world) {
    super(world,"man");
  }
  
  public Man(World world,String name) {
    super(world,name);
  }
  
  public void run() {
    center();
    setX(1.0);
    incRotY(-1.2);
    sleep();sleep();
    //setAuto(1);
    act();
  }
}

class Woman extends Man {
  public Woman(World world) {
    super(world,"woman");
    scaleTo(4);           
  }

  public void run() {
    center();
    setX(-1.0);
    setY(0.5);
    incRotY(2.5);
    
    sleep();sleep();
    //setAuto(1);
    act();
  }
}

class Model extends Body implements Runnable {
  public Model(World world,String[] FileOrURLs,Object data) {
    super(world,FileOrURLs,data);
     scaleTo(0.3);
  }
  
  public void center() {
    setFocus(null);
    //setY(-1.0);
    //setZ(-2.0);
    //scaleTo(0.3);
    sleep();
  }
  
  public void run() {
    center();
    //setAuto(1);
  }
  
  public void animate() {
    Thread T=new Thread(this,"Prolog3DThread");
    T.setDaemon(true);
    T.start();
  }
}
 
class Animal extends Body implements Runnable,Serializable {
  public Animal(World world,String[] FileOrURLs,Object data) {
    super(world,FileOrURLs,data);
    scaleTo(0.3);
  }

  public void center() {
    setFocus(null);
    sleep();
  }
  
  public void run() {
    System.out.println("running, running!");
    center();
    //setAuto(1);
  }
  
  public void animate() {
    Thread T=new Thread(this,"Prolog3DThread");
    T.setDaemon(true);
    T.start();
  }

  public static boolean save(Animal A,String file) {
    try {
      FileOutputStream fs=new FileOutputStream(file);
      ObjectOutputStream os=new ObjectOutputStream(fs);
      os.writeObject(A);
      os.close();
      return true;
    }
    catch(Exception e) {
    }
    return false;
  }

  public static Animal load(String file) {
    try {
      FileInputStream fs=new FileInputStream(file);
      ObjectInputStream is=new ObjectInputStream(fs);
      Animal A=(Animal)is.readObject();
      is.close();
      return A;
    }
    catch(Exception e) {
    }
    return null;
  }

  public static void zootest() {
    Prolog3D M=new Prolog3D();
    World world=M.getWorld();
    Animal anAnimal= new Octo(world,"Ralph");
    M.addControls();
    M.showWorld(400,400);
    anAnimal.animate();
    Params.sleep(20000);
    M.stopWorld();
    M.exit();
  }
}

class Octo extends Animal {
  public Octo(World world,Object name) {
    super(world, new String[]{"models/octopus.j3f.gz","/paul/models/octopus.j3f.gz"},name);
    scaleTo(0.2);
  }

  public Octo(World world) {
    this(world,"Otto");
  }
  
  public void run() {
    setZ(-1.0);
    for(int k=0;k<3;k++) {
      turn_left();
      for(int i=0;i<5;i++) {
        walk();
        //sleep();
      }
    }
  }
}
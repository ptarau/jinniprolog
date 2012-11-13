package prolog3d;

import javax.media.j3d.*;
import javax.vecmath.*;
import java.util.*;
import java.awt.event.*;
import java.awt.*;
import com.sun.j3d.utils.picking.*;

/**
  Generic agents - designed as behaviors to which Shapes can be added.
 */
public class Agent3D extends Behavior 
{
  
  /**
     Creates a new agent and inserts it in a given world
     as a toplevel entitiy if the boolean "top" is true.
     At his point an agent is "pure behavior" - note that no 
     shape has yet been attached to the agent - this is usually 
     done by the classes that extend this base class.
   */
  public Agent3D(World world,Object data,boolean top) {
    super();
    trans=new Transform3D();
    tg=new TransformGroup();
    setSchedulingBounds(World.bounds);

    if(null==data) data="!?";
    this.world=world;
    this.data=data;
    reset();
    
    tg.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
    tg.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
    tg.addChild(this); // a behavior !!!

    if(top) world.getScene().addChild(tg);
    addToCat();
  }
  
  /**
   Creates a new agent and inserts it in a given world
   as a toplevel entity.
  */  
  public Agent3D(World world,Object data) {
    this(world,data,true);
  }

  /**
    World containing the agent.
   */
  private final World world;
  protected Object data;
  
  private int auto=0;
  
  /**
     Transform that applies translations,scale and rotation operations
     to the transform group of this agent.
  */
  final private Transform3D trans; //=new Transform3D();
  
  /**
     This TransformGroup is the toplevel container for
     the visual components of this agent.
  */
  final private TransformGroup tg;//=new TransformGroup();
  
  /**
     The condition controlling the reaction to visual and mouse
     events.
   */
  private WakeupCondition condition;
  
  private Vector3f pos;
  private Vector3d rot;
  private Vector3d scale;
  private Vector3d oldscale;
  
  void initFromTG(TransformGroup tgroup) {
    Transform3D T=new Transform3D();
    tgroup.getTransform(T);
    T.get(this.pos);
    T.getScale(scale);
    
    Quat4f Q=new Quat4f();
    T.get(Q);  
    AxisAngle4f A=new AxisAngle4f();
    A.set(Q);
    this.rot.x=A.angle*A.x;
    this.rot.y=A.angle*A.y;
    this.rot.z=A.angle*A.z;
    //Prolog3D.pp("quat="+Q); Prolog3D.pp("axis="+A);Prolog3D.pp("rot="+rot);
  }
  
  /**
     Returns the World to which this agent belongs.
   */
  public World getWorld() {
    return world;
  }
  
  /**
     Adds this agent to a Jinni Cat - a class containing
     a graph with multiple edges (morphisms) and attributes.
     The Cat class extends Cat so a Cat object
     also supports graph ranking algorithms and computation of
     connected components.
   */
  public void addToCat() {
    getWorld().getAgents().setProp(this,"data",data);
  }
  
  /**
     Returns the data attached to this agent seen as a vertex
     in a graph.
   */
  public Object getData() {
    return getWorld().getAgents().getProp(this,"data");
  }

  /**
     Gets the top TransformGroup containig visual components of this agent.
   */
  public TransformGroup getGroup() {
    return tg;
  }

  //public void setGroup(TransformGroup tg) {this.tg=tg;}
  
  /**
     Adds a visual component (a child Node) to this agent.
   */
  public void addChild(Node N) {
    tg.addChild(N);
  }

  public void print(Object O) {
    getWorld().print(O);
  }
  
  /**
     Returns the tranlation component (position from origin) of this agent.
   */
  public Vector3f getPos() {
    return pos;
  }

  /**
     Initializes this agent (required by superclass Behavior)
   */
  public void initialize() {
    setWakeUp(Params.tick);
    wakeupOn(condition);
  }
  
  /**
     Sets the event handling mechanism for this agent by specifying
     what type of events (visual, mouse etc.) will be handled.
   */
  public void setWakeUp(int tick) {
    setWakeUp1(tick);
  }
  
  /**
     Wake-up only on visual events
   */
  public void setWakeUp1(int tick) {
    condition=new WakeupOnElapsedTime(tick);
  }

  /**
    Wake-up on visual and mouse events.
  */
  public void setWakeUp2(int tick) {
    WakeupCriterion[] criterions=new WakeupCriterion[2];
    criterions[0]=new WakeupOnElapsedTime(tick);
    if(null==criterions[1]) criterions[1]=new WakeupOnAWTEvent(MouseEvent.MOUSE_CLICKED);
    condition=new WakeupOr(criterions);
  }
  
  /**
   Implements the Agents action on various events
   */  
  public void processStimulus(Enumeration criteria) {
    //Prolog3D.pp("entering:"+this+" ,stopAll="+Params.stopAll);
    if(Params.stopAll) {
      //Prolog3D.pp("stopping:"+this);
      condition=null;
      return;
    }
    while(criteria.hasMoreElements()) {
      WakeupCriterion criterion=(WakeupCriterion)criteria.nextElement();
      if(criterion instanceof WakeupOnElapsedTime) {
        applyStep();
      }
      else if(criterion instanceof WakeupOnAWTEvent) {
        processAWTEvents((WakeupOnAWTEvent)criterion);
      }
      else
        Prolog3D.pp("Unexpected wakeUpOn="+criterion.getClass());
    }
    wakeupOn(condition);
  }
  
  /**
     Action on Mouse Events
   */
  public void processAWTEvents(WakeupOnAWTEvent criterion) {
    //Prolog3D.pp("AWTEvent in:"+this+" ,stopAll="+Params.stopAll);
    if(Params.stopAll) return;
    AWTEvent[] events=((WakeupOnAWTEvent)criterion).getAWTEvent();
    for (int i=0; i<events.length; i++) { 
      AWTEvent event=events[i];
      int eventId = event.getID();         
      if(eventId==MouseEvent.MOUSE_CLICKED) {
        MouseEvent e=(MouseEvent)event;  
        world.getPickCanvas().setShapeLocation(e);
        PickResult result=null;
        try {
          result = world.getPickCanvas().pickClosest();
        } catch(CapabilityNotSetException ex) {
          Prolog3D.pp("warning: capabilities missing for picking this object");
        }
        if(null==result) continue;
        //Node N=result.getObject();
        Node N=result.getNode(PickResult.TRANSFORM_GROUP);
        if(null==N) continue;
        checkEventOnShape(N,e);  
      }
      else
        Prolog3D.pp("unexpected Mouse event:"+events[i].getClass());
    }
    
  }

  /**
     Usually overridden. Handles events like mouse clicks on various shapes.
   */
  public void checkEventOnShape(Node N,MouseEvent e) {}

  private void applyStep() {
    transform();
    trans.setEuler(rot);
    trans.setScale(scale);
    trans.setTranslation(pos);
    applyTransform();
  }
  
  /**
     Implementes various transformations specific to each agent,
     rotation, scale, translation.
   */
  public void transform() {
    if(auto>0) runAuto();
    //Prolog3D.pp(this+">>>"+rot);
  }
  
  synchronized private void applyTransform() {
    transformFocus();
  } 

  public void transformFocus() {
    applyTransformTo(tg);
    TransformGroup wg=world.getView(this);
    if(null!=wg) applyTransformTo(wg);
  }
  
  public void applyTransformTo(TransformGroup tgroup) {
    tgroup.setTransform(trans);
  }
  
  /**
    (Re)inititalizes an Agent with default paprameters.
   */
  public void reset() {
    pos=new Vector3f(0,0,0);
    rot=new Vector3d(0,0,0);
    oldscale=new Vector3d(1,1,1); // should be 1!!!
    scale=new Vector3d(1,1,1); // should be 1!!!
    auto=0;
    hiding=false;
  }
  
  public void center() {
    setX(0);
    setY(0);
    setZ(0);
  }

  /**
    stops an agents by ensuring that it will not be 
    scheduled to handle any events in the future
   */
  public void stop() {
    reset();
    hideAgent();
    condition=null;
  }
  
  private boolean hiding=false;
  
  /**
     Makes an agent invisible.
   */
  public void hideAgent() {
    if(hiding) return;
    hiding=true;
    oldscale.x=scale.x;oldscale.y=scale.y;oldscale.z=scale.z;
    scaleTo(0.00001);
    //setWakeUp(1000);
  }

 /**
     Makes an agent visible.
  */
  public void showAgent() {
    if(!hiding) return;
    hiding=false;
    scaleTo(oldscale.x,oldscale.y,oldscale.z);
  }
  
  /*
   API: move/rotate/scale the agent
   */
   
   /**
      Move this agent to a given position
    */
  public void moveTo(double x,double y,double z) {
    pos.x=(float)x;
    pos.y=(float)y;
    pos.z=(float)z;
  }
  
  /**
     Rotates this agent using Euler angles describing how
     much it rotates upon the x,y,z axes.
   */
  public void rotateTo(double x,double y,double z) {
    rot.x=x;
    rot.y=y;
    rot.z=z;
  }
  
  /**
     Scales this agent by given x,y,z factors.
   */
  public void scaleTo(double x,double y,double z) {
    scale.x=x;
    scale.y=y;
    scale.z=z;
  }
  
  /**
     Scales this agent uniformly.
   */
  public void scaleTo(double s) {
    scaleTo(s,s,s);
  }
  
  public void setX(double x) {
    pos.x=(float)x;
    //Prolog3D.pp("pos="+pos);
  }
  
  public void setY(double y) {
    pos.y=(float)y;
  }
  
  public void setZ(double z) {
    pos.z=(float)z;
  }
  
  
  public void incX(double x) {
    pos.x+=(float)x;
  }
  
  public void incY(double y) {
    pos.y+=(float)y;
  }
  
  public void incZ(double z) {
    pos.z+=(float)z;
  }
  
  public void incRotX(double angle) {
    rot.x+=angle;
  }
  
  public void incRotY(double angle) {
    rot.y+=angle;
  }
  
  public void incRotZ(double angle) {
    rot.z+=angle;
  }
  
  
  public void setRotX(double angle) {
    rot.x=angle;
  }
  
  public void setRotY(double angle) {
    rot.y=angle;
  }
  
  public void setRotZ(double angle) {
    rot.z=angle;
  }
  
  public double getX() {return pos.x;}
  public double getY() {return pos.y;}
  public double getZ() {return pos.z;}
  public double getRotX() {return rot.x;}
  public double getRotY() {return rot.y;}
  public double getRotZ() {return rot.z;}
  public double getScaleX() {return scale.x;}
  public double getScaleY() {return scale.y;}
  public double getScaleZ() {return scale.z;}
  
  
  /*
     high level i.e. somewhat anthropomorphic moving comands
  */
  
  private int sleep_ms=Math.min(10,Math.max(1,(10-Params.speed)))*Params.tick;
  
  /**
     Accelerates an agent by reducing sleep time between
     animation steps. Ensures that sleep time is never lower
     than the double of the the interval the update mechanism
     uses (defined by Params.tick) to apply changes. Unless
     a machine is extremely slow, this means that animation
     steps are all executed.
  */
  public void hurry() {
    Prolog3D.pp("hurrying: sleep_ms="+sleep_ms);
    if(sleep_ms>2*Params.tick) sleep_ms-=Params.tick;
  }
  
  /**
     slows down an agent by increasing sleep time between animation steps
  */
  public void relax() {
    if(sleep_ms<20*Params.tick) sleep_ms+=Params.tick;
  }
  
  /**
     sleeps for an interval appropriate for the changes
     described in an animation step to take effect
   */
  public void sleep() {
    Params.sleep(sleep_ms);
  }
  
  private double delta=0.1;
  private double alpha=Math.PI/12;
  
  /**
   * sets delta used by one walk() call - default 0.1 units
   */
  public void setStep(double delta) {
    this.delta=delta;
  }

  /**
   * sets alpha used by one turn or tilt step - default 30 degrees = "1 hour"
   */
  public void setTurn(int degrees) {
    this.alpha=degrees*(Math.PI/180);
  }

  /**  
     Sets the focus to a named TransformGroup which
     is part of this agent or the agent's top TransformGroup
     if the parameter is null.
     
  */
  public boolean setFocus(String name) {
    //nothing to do here - but see overridings in Body.java
    return true;
  }
  
  /**
     Simple walk routine expressed as translation in a horizontal plane.
     Agents should override this for more relaistic animations.
   */
  public void walk() {
    setFocus(null);

    double a=getRotY();
    incX(delta*Math.sin(a));
    incZ(delta*Math.cos(a));
   
    double b=getRotX();
    incY(-delta*Math.sin(b));
    incZ(delta*Math.cos(b));
   
    //Prolog3D.pp("a="+getRotY()+",x="+getX()+",z="+getZ());

    sleep();
  }
  
  /**
     Turns left by alpha (default 1/12 of a circle - corresponding to 1 hour
     on an analog clock)
   */
  public void turn_left() {
    incRotY(alpha);
    sleep();
  }
  
  /**
   Turns right by alpha (default 1/12 of a circle - corresponding to 1 hour
   on an analog clock)
   */
  public void turn_right() {
    incRotY(-alpha);
    sleep();
  }
  
  /**
   Turns back (half of a circle rotation upon the Y axis)
   */
  public void turn_back() {
    incRotY(Math.PI);
    sleep();
  }
  
  /**
   Tilts left by alpha (default 1/12 of a circle - corresponding to 1 hour
   on an analog clock rotation upon the Z axis) 
   */
  public void tilt_left() {
    incRotZ(-alpha);
    sleep();
  }
  
  /**
   Tilts right by alpha (default 1/12 of a circle - corresponding to 1 hour
   on an analog clock rotation upon the Z axis) 
   */
  public void tilt_right() {
    incRotZ(alpha);
    sleep();
  }
  
  /**
   Turns down by alpha (default 1/12 of a circle - corresponding to 1 hour
   on an analog clock rotation upon the X axis) 
   */
  public void turn_down() {
    incRotX(alpha);
    sleep();
  }
 
  /**
   Turns upward by alpha (default 1/12 of a circle - corresponding to 1 hour
   on an analog clock rotation upon the X axis) 
   */ 
  public void turn_up() {
    incRotX(-alpha);
    sleep();
  }
  
  /**
     Puts the agent on some random automatic behavior.
   */
  public void setAuto(int auto) {
    this.auto=auto;
  }
  
  /**
     Implements a random behavior consisting of small
     rotations and translations
   */
  public void runAuto() {
  
    //Prolog3D.pp("auto:"+this);
    switch(auto) {
      case 0: /*do nothing*/ break;
      case 1: autoMove(); break;
      case 2: autoColor(); break;
      case 3: autoMove();autoColor(); break;
      default: /* do nothing */
    }
  }
  
  public void autoMove() {
    int choice=Params.ri(6);
    switch(choice) {
      case 0:
        setX(getX()+rf(0.1));
        break;
      case 1:
        setY(getY()+rf(0.1));
        break;
      case 2:
        setZ(getZ()+rf(0.1));
        break;
      case 3:
        setRotX(getRotX()+rf(0.2));
        break;
      case 4:
        setRotY(getRotY()+rf(0.2));
        break;
      case 5:
        setRotZ(getRotZ()+rf(0.2));
        break;
      default: {
        double s=0.1;
        scaleTo(1f+rf(s),1f+rf(s),1f+rf(s));
      }
    }
  }

  public void autoColor() {
  }

  /**
     Generates random float from -r to r
   */
  public static float rf(double r) {
    return Params.rf((float)r);
  }
  
  /**
    Provides a readable representation of the state of this agent.
   */
  public String toString() {
    Object data=getData();
    if(null==data) return "vertex at: [pos"+pos+",rot"+rot+",scale"+scale+"]";
    else return data.toString();
  }
}

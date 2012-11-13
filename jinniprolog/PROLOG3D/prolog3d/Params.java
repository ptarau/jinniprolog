package prolog3d;

import prolog.core.Cat; // ?!

import javax.media.j3d.*;
import javax.vecmath.*;
import java.util.*;
import java.io.*;
import java.beans.XMLDecoder;

public class Params {
  public Params() {
  }
  
  public static boolean reuseTopWindow=false;
  public static String winTitle="Prolog3D";
  public static int layoutSleep=10;
  public static int tick=40;
  public static int speed=5;
  public static int printH=240;
  public static int printW=240;
  
  public static Color3f textColor=new Color3f(0.6f,0.0f,0.0f);
  public static Appearance textApp=Simple.makeApp(textColor,0.0f);
  public static int maxText=20;
  public static boolean stopAll=false;  
  static private boolean applet=false;
  public static int verbose=0;
  public static int interactive=1;

  public static String bgfile="bg.jpg";
  
  public static Color3f bgColor = new Color3f(1, 1, 1);
  public static Color3f bgLightColor = new Color3f(0.5f, 0.5f, 0.5f);
  public static Color3f bgAmbientColor = new Color3f(0.5f, 0.5f, 0.5f);
  public static Vector3f bgLightDir  = new Vector3f(16.0f, -16.0f, -16.0f);
  
  static public void setBgColor(double r,double g,double b) {
    bgColor=new Color3f((float)r, (float)g, (float)b);
  }

  static public void setApplet() {
    applet=true;
  }  
  
  static public void setInteractive(int i) {
    interactive=i;
  }  

  static public boolean isApplet() {
    return applet;
  }  
    
  static Random random=new Random();
  
  public static int ri(int max) {
    return random.nextInt(max);
  }
  
  public static final float rf() {
    return rf(1);
  }
  
  public static final float rf(float r) {
    return 2*r*random.nextFloat()-r;
  }
  
  public static final float rf01() {
    return random.nextFloat();
  }
  
  public static final float r(float max) {
    return random.nextFloat()*max;
  }
  
  public static Point3f rs(float r) {
    float a=0;
    float b=0;
    int max=100;
    for(int i=0;i<max;i++) {
      a=rf(1);
      b=rf(1);
      if(a*a+b*b < 1) break;
      if(i==max-1) return null;
    }
    float c=a*a+b*b;
    float s=(float)Math.sqrt(1-c);
    float x=2*a*s;
    float y=2*b*s;
    float z=1-2*c;
    return new Point3f(x*r,y*r,z*r);
  } 

  public static Cat randomCat(int seed,int v0,int v,int e0,int e) {
    Cat RG=new Cat();
    RG.randomize(seed,v0+ri(v),e0+ri(e));
    return RG;
  }
     
  public static Cat randomRanked(int seed,int v0,int v,int e0,int e,int giant,int m) {
    Cat RG=randomCat(seed,v0,v,e0,e);
    Prolog3D.pp(Cat.showInfo(RG));
    if(giant>0) RG=(Cat)RG.trimRankedGraph(giant,m);
    //if(g>0) RG=(Cat)RG.trim(g>0,m);
    return RG;
  }
  
  static public void sleep(long ms) {
    try {
      Thread.sleep(ms);
    } 
    catch (InterruptedException e) {}
  }
}

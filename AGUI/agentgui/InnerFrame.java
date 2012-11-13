package agentgui;

import javax.swing.JInternalFrame;
import javax.swing.JInternalFrame;
import javax.swing.JDesktopPane;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JMenuBar;
import javax.swing.JFrame;
import javax.swing.KeyStroke;

import java.awt.event.*;
import java.awt.*;

import java.util.Random;

/* Used by InternalFrameDemo.java. */
public class InnerFrame extends JInternalFrame {
   public static int openFrameCount = 0;
   public static final int xOffset = 30, yOffset = 30;
   public static int def_x=480;
   public static int def_y=320;
   public static int def_d=100;
  
   public static int rand() {
	   return def_d+(new Random().nextInt()) % def_d;
   }
   
   public InnerFrame() {
	   this("");
   }
   
   public InnerFrame(String title)  { 
	   this(title,def_x,def_y);
	   //System.err.println(title+" x="+def_x+",y="+def_y);
   }
   
   public InnerFrame(String title,int dx,int dy)  {
	   this(title,rand(),rand(),dx,dy);
   }
   
   
   public InnerFrame(String title,int x,int y,int dx,int dy) {
       super(title, 
             true, //resizable
             true, //closable
             true, //maximizable
             true);//iconifiable
       setSize(dx,dy);
       setLocation(x,y);
       setVisible(true);
       ++openFrameCount;
   }
   
}
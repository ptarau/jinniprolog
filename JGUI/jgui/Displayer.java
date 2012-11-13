package jgui;

import javax.swing.*;
import javax.swing.text.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.text.*;
import javax.swing.text.html.*;
import java.io.StringReader;
import java.io.IOException;

import prolog.logic.Prolog;
import prolog.kernel.TextSink;
import prolog.kernel.Machine;

public class Displayer extends JScrollPane implements TextSink,ComponentListener {
  public static final int minH=20;
  public static final int minW=80;
  public static int max_display=600;
  
  public static void set_max_display(int max) {
     max_display=3*max;
  }
  
  private JEditorPane textPane;

  //#private JScrollPane paneScrollPane;

  public void componentHidden(ComponentEvent e) {
    //Prolog.dump("Displayer:"+e.getComponent().getClass().getName()+" --- Hidden");
  }

  public void componentMoved(ComponentEvent e) {
    //Prolog.dump("Displayer:"+e.getComponent().getClass().getName()+" --- Moved");
  }

  public void componentShown(ComponentEvent e) {
    //Prolog.dump("Displayer:"+e.getComponent().getClass().getName()+" --- Shown");
  }
  
  public void componentResized(ComponentEvent e) {
    JComponent C=(JComponent)e.getComponent();
    smartResize(C,this,0,0);
    smartResize(this,textPane,0,0); //#
    //#smartResize(paneScrollPane,textPane,0,0);
  }
  
  public static Dimension smartResize(JComponent P,JComponent C,double dh,double dw) {
    Dimension D=P.getSize();
    Dimension MD=C.getMinimumSize();
    
    int w=D.width;
    int h=D.height;
    
    w-=(int)(w*dw);
    h-=(int)(h*dh);
    
    h=Math.max(h,MD.height);
    w=Math.max(w,MD.width);
    
    C.setMaximumSize(D);
    
    D.setSize(w-dw*w,h-dh*h);
    C.setPreferredSize(D);
   
    C.revalidate();
    C.repaint();
     
    //Prolog.dump("Displayer:"+C.getClass().getName()+" --- smartResize "+C.getSize());
    
    return D;
  }

  public void setText(final String s) {
    //System.err.println(this.hashCode()+"Displayer.setText:"+s);
    set_text(s);
  }
  
  public String getText() {
    return get_text();
  }
  
  private final void set_text0(final String s) {
    this.textPane.setText(s);
  }
  
  public void set_text(final String s) {
    Start.invokeLater(new Runnable(){
      public void run() {
        set_text0(s);
      }
    });
    
    scrollPaneToBottom();
  }

  public String get_text() {
    return textPane.getText();
  }

  ////  
  private int append_count=0;
  
  public void append_text(final String s) {
   
    Start.invokeLater(new Runnable(){
      public void run() {
        ++append_count;
        if(append_count > max_display) {
          append_count=0;
          set_text0(s);
        }
        else {
          append_text0(s);
        }
      }
    });
    
    scrollPaneToBottom();
    
  }
  
  private final void append_text0(String s) {
    try {
      if("text/plain".equals(textPane.getContentType())) {
        Document doc=textPane.getDocument();
        doc.insertString(doc.getLength(),s,null);
      } else {
        appendHTML(textPane,s);
      }
    } catch(BadLocationException e) {
      e.printStackTrace();
    } catch(IOException e) {
      e.printStackTrace();
    }
  }

  private final void scroll0() {
    getVerticalScrollBar().setValue(
        getVerticalScrollBar().getMaximum());
  }
  
  ////
  private void scrollPaneToBottom() {
    Start.invokeLater(new Runnable(){
      public void run() {
        scroll0();
      }
    });
  }

  public Displayer(final int width,final int height){
    this(width,height,"text/html");
  }
 
  public Displayer(final int w,final int h,String contentType){
    super();
    this.textPane=makeTextPane(w,h,contentType);
    setViewportView(textPane);
    int mw=Math.max(w,minW);
    int mh=Math.max(h,minH);
    this.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
    this.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
    setDims(mw,mh);
  }

  private static JEditorPane makeTextPane(int w,int h,String contentType) {
    int mw=Math.max(w,minW);
    int mh=Math.max(h,minH);
    JEditorPane textPane=new JEditorPane();
    textPane.setMinimumSize(new Dimension(minW,minH));
    textPane.setPreferredSize(new Dimension(mw,mh));
    textPane.setMaximumSize(new Dimension(mw,mh));
    // textPane.setEditable(false);
    textPane.setContentType(contentType);
    return textPane;
  }
  
  public void setDims(int w,int h) {
    setMinimumSize(new Dimension(10+minW,10+minH));
    Dimension D=new Dimension(w,h);
    setPreferredSize(D);
    setMaximumSize(D);
  }

  private void appendHTML(JEditorPane editor,String html) throws IOException,
      BadLocationException {
    // assumes editor is already set to "text/html" type
    HTMLEditorKit kit=(HTMLEditorKit)editor.getEditorKit();
    Document doc=editor.getDocument();
    StringReader reader=new StringReader(html);
    kit.read(reader,doc,doc.getLength());
  }

  /*
   * protected void addStylesToDocument(StyledDocument doc) { //Initialize some
   * styles. Style def=StyleContext.getDefaultStyleContext().
   * getStyle(StyleContext.DEFAULT_STYLE);
   * 
   * Style regular=doc.addStyle("regular",def);
   * StyleConstants.setFontFamily(def,"SansSerif");
   * 
   * Style s=doc.addStyle("italic",regular); StyleConstants.setItalic(s,true);
   * 
   * s=doc.addStyle("bold",regular); StyleConstants.setBold(s,true);
   * 
   * s=doc.addStyle("small",regular); StyleConstants.setFontSize(s,10);
   * 
   * s=doc.addStyle("large",regular); StyleConstants.setFontSize(s,16);
   * 
   * s=doc.addStyle("icon",regular);
   * StyleConstants.setAlignment(s,StyleConstants.ALIGN_CENTER);
   * 
   * s=doc.addStyle("button",regular);
   * StyleConstants.setAlignment(s,StyleConstants.ALIGN_CENTER); }
   */

  
  public void test_text() {
    set_text(sampleText);
  }

  public static Displayer showText(final String title,final String initialText,
      final int w,final int h) {

    JFrame frame=new JFrame(title);
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

    Displayer displayer=new Displayer(w,h,"text/html");

    System.err.println("Displayer1:\n"+initialText);

    displayer.set_text(initialText+'\n');

    frame.setContentPane(displayer);

    frame.pack();
    frame.setVisible(true);

    System.err.println("created Displayer");
    return displayer;
  }

  public static Displayer showText(String initialText) {
    String title="Prolog Display Agent";
    return showText(title,initialText,640,400);
  }

  public static Displayer showText() {
    return showText(sampleText);
  }

  public static String sampleText="<html>\n"+"Color and font test:\n"+"<ul>\n"
      +"<li><font color=red>red</font>\n"+"<li><font color=blue>blue</font>\n"
      +"<li><font color=green>green</font>\n"
      +"<li><font size=-2>small</font>\n"+"<li><font size=+2>large</font>\n"
      +"<li><i>italic</i>\n"+"<li><b>bold</b>\n"+"</ul>\n"+"</html>\n";

  public static void main(String[] args) {
    System.err.println("testing Displayer");
    showText();
  }
  
}

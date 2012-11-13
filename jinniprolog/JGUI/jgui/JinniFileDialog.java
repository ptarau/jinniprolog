package jgui;

import javax.swing.*;
//import javax.swing.event.*;
//import javax.swing.text.*;
//import javax.swing.filechooser.*;

import java.io.File;


public class JinniFileDialog extends JFileChooser  {
  
public JinniFileDialog(JinniFrame F,String name,int mode,String filter) {
    //super(F,name,mode);
    super("."); //(File)null,(FileSystemView)null);
    this.mode=mode;
    setDialogTitle(name);
    setDialogType(mode);
    //this.filter=filter;
    //setFilenameFilter(this);
    Start.setLooks(this);
    
  }
  private int mode;
  //private String filter;
  
  public String getChoice() {
     int status;
     if(0==mode) 
       status=showOpenDialog(null);
     else  
       status=showSaveDialog(null);
     File selectedFile = getSelectedFile();
     if(null==selectedFile) return null;
     return selectedFile.getName();
  }
  
  public boolean accept(File dir, String name) {
    //Prolog.dump("accept called with: "+name);
    //return name.endsWith("."+this.filter);
    return true; // this makes behavior uniform accross platforms
  }
}


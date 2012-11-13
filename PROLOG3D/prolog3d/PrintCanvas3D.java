package prolog3d;

/*
 *	@(#)PrintCanvas3D.java 1.5 02/04/01 15:04:11
 *
 * Copyright (c) 1996-2002 Sun Microsystems, Inc. All Rights Reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 *
 * - Redistribution in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in
 *   the documentation and/or other materials provided with the
 *   distribution.
 *
 * Neither the name of Sun Microsystems, Inc. or the names of
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 *
 * This software is provided "AS IS," without a warranty of any
 * kind. ALL EXPRESS OR IMPLIED CONDITIONS, REPRESENTATIONS AND
 * WARRANTIES, INCLUDING ANY IMPLIED WARRANTY OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE OR NON-INFRINGEMENT, ARE HEREBY
 * EXCLUDED. SUN AND ITS LICENSORS SHALL NOT BE LIABLE FOR ANY DAMAGES
 * SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR
 * DISTRIBUTING THE SOFTWARE OR ITS DERIVATIVES. IN NO EVENT WILL SUN
 * OR ITS LICENSORS BE LIABLE FOR ANY LOST REVENUE, PROFIT OR DATA, OR
 * FOR DIRECT, INDIRECT, SPECIAL, CONSEQUENTIAL, INCIDENTAL OR
 * PUNITIVE DAMAGES, HOWEVER CAUSED AND REGARDLESS OF THE THEORY OF
 * LIABILITY, ARISING OUT OF THE USE OF OR INABILITY TO USE SOFTWARE,
 * EVEN IF SUN HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
 *
 * You acknowledge that Software is not designed,licensed or intended
 * for use in the design, construction, operation or maintenance of
 * any nuclear facility.
 
 * major changes - Paul Tarau 2004
 */

import javax.swing.*;
import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GraphicsConfiguration;
import java.awt.Point;
import java.awt.image.BufferedImage;
import java.awt.event.*;
import java.awt.print.*;

import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.image.ImageObserver;
import java.awt.geom.AffineTransform;
import java.awt.Color;

import javax.media.j3d.*;
import javax.vecmath.*;
import com.sun.j3d.utils.behaviors.mouse.*;
import com.sun.j3d.utils.universe.*;

import java.io.*;
import javax.imageio.*;

public class PrintCanvas3D extends JFrame implements ActionListener {

  
  public static void print(BranchGroup b,int w,int h) {
    if(null!=b) new PrintCanvas3D(b,w,h);
    else Prolog3D.pp("error: nothing to print!!!");
  }
  
  private JMenuItem snapshotItem;
  private JMenuItem printItem;
  private JMenuItem quitItem;

  private SimpleUniverse u;
  private Canvas3D canvas3D;
  private OffScreenCanvas3D offScreenCanvas3D;

  private static final int OFF_SCREEN_SCALE = 3;

  // Create the Canvas3D (both on-screen and off-screen)
  private void createCanvas3D(BranchGroup scene,int w,int h) {
 
    // Create Canvas3D
    GraphicsConfiguration config =
      SimpleUniverse.getPreferredConfiguration();

    canvas3D = new Canvas3D(config);
    canvas3D.setSize(w, h);

    // Create the off-screen Canvas3D object
    offScreenCanvas3D = new OffScreenCanvas3D(config, true);
    // Set the off-screen size based on a scale factor times the
    // on-screen size
    Screen3D sOn = canvas3D.getScreen3D();
    Screen3D sOff = offScreenCanvas3D.getScreen3D();
    Dimension dim = sOn.getSize();
    dim.width *= OFF_SCREEN_SCALE;
    dim.height *= OFF_SCREEN_SCALE;
    sOff.setSize(dim);
    sOff.setPhysicalScreenWidth(sOn.getPhysicalScreenWidth() *
      OFF_SCREEN_SCALE);
    sOff.setPhysicalScreenHeight(sOn.getPhysicalScreenHeight() *
      OFF_SCREEN_SCALE);


    // Create a simple scene and attach it to the virtual universe
    u = new SimpleUniverse(canvas3D);
    // This will move the ViewPlatform back a bit so the
    // objects in the scene can be viewed.
    u.getViewingPlatform().setNominalViewingTransform();
    u.addBranchGraph(scene);
    // attach the offscreen canvas to the view
    u.getViewer().getView().addCanvas3D(offScreenCanvas3D);
  }


  private class AppPanel extends JPanel {
    private AppPanel(BranchGroup scene,int w,int h) {
      setLayout(new BorderLayout());

      // Create Canvas3D and scene graph
      createCanvas3D(scene,h,w);
      add("Center", canvas3D);
    }
  }

  public static int image_cnt=0;
  
  public void actionPerformed (ActionEvent event) {
    Object target = event.getSource();

    if ((target == snapshotItem) || (target == printItem)) {
      BufferedImage bImage=makeImage(this.canvas3D,this.offScreenCanvas3D);
      if (target == snapshotItem) {
        image2file(bImage,"image"+(++image_cnt)+".png");
        new ImageDisplayer(bImage);
      }
      else { // (target == printItem)
        new ImagePrinter(bImage).print();
      }
    }
    else if (target == quitItem) {
      stop();
    }
  }

  public void stop() {
    Prolog3D.pp("stopping PrintCanvas");
    u.removeAllLocales();
    dispose();
    //Prolog3D.stopWorld();
    Prolog3D.pp("stopped PrintCanvas");
  }
  
  static void image2file(BufferedImage bImage,String fname) {
    try {
      ImageIO.write(bImage, "png", new File(fname));
      //buggy ImageIO.write(bImage, "JPEG", new File("image.jpg"));
    }
    catch(IOException e) {
    }  
  }

  public static void canvas2file(Canvas3D c,OffScreenCanvas3D o,String fname) {
    image2file(makeImage(c,o),fname);
  }
  
  static BufferedImage makeImage(Canvas3D canvas3D, OffScreenCanvas3D offScreenCanvas3D) {
    Point loc = canvas3D.getLocationOnScreen();
    offScreenCanvas3D.setOffScreenLocation(loc);
    Dimension dim = canvas3D.getSize();
    dim.width *= OFF_SCREEN_SCALE;
    dim.height *= OFF_SCREEN_SCALE;
    BufferedImage bImage =
      offScreenCanvas3D.doRender(dim.width, dim.height);
    return bImage;
  }
  
  private JMenuBar createMenuBar() {
    JMenuBar menuBar = new JMenuBar();
    JMenu fileMenu = new JMenu("File");
    snapshotItem = new JMenuItem("Snapshot");
    snapshotItem.addActionListener(this);
    printItem = new JMenuItem("Print...");
    printItem.addActionListener(this);
    quitItem = new JMenuItem("Quit");
    quitItem.addActionListener(this);
    fileMenu.add(snapshotItem);
    fileMenu.add(printItem);
    fileMenu.add(new JSeparator());
    fileMenu.add(quitItem);
    menuBar.add(fileMenu);
    return menuBar;
  }

  private PrintCanvas3D(BranchGroup scene,int w,int h) {
    this.setTitle("Canvas3D Printer");

    // Create and initialize menu bar
    JPopupMenu.setDefaultLightWeightPopupEnabled(false);
    this.setJMenuBar(createMenuBar());

    // Handle the close event
    this.addWindowListener(new WindowAdapter() {
      public void windowClosing(WindowEvent winEvent) {
        stop();
      }
    });

    // Add main panel to top-level frame and make it visible
    this.getContentPane().add(new AppPanel(scene,w,h));
    this.pack();
    this.setVisible(true);
  }
}

class OffScreenCanvas3D extends Canvas3D {
  OffScreenCanvas3D(GraphicsConfiguration graphicsConfiguration,
    boolean offScreen) {

    super(graphicsConfiguration, offScreen);
  }

  BufferedImage doRender(int width, int height) {

    BufferedImage bImage =
      new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);

    ImageComponent2D buffer =
      new ImageComponent2D(ImageComponent.FORMAT_RGBA, bImage);

    setOffScreenBuffer(buffer);
    renderOffScreenBuffer();
    waitForOffScreenRendering();
    bImage = getOffScreenBuffer().getImage();

    return bImage;
  }

  public void postSwap() {
    // No-op since we always wait for off-screen rendering to complete
  }
}


class ImagePrinter implements Printable, ImageObserver {
  BufferedImage bImage;

  public int print(Graphics g, PageFormat pf, int pi)
    throws PrinterException {


    if (pi >= 1) {
      return Printable.NO_SUCH_PAGE;
    }

    Graphics2D g2d = (Graphics2D)g;
    //g2d.translate(pf.getImageableX(), pf.getImageableY());
    AffineTransform t2d = new AffineTransform();
    t2d.translate(pf.getImageableX(), pf.getImageableY());
    double xscale  = pf.getImageableWidth() / (double)bImage.getWidth();
    double yscale  = pf.getImageableHeight() / (double)bImage.getHeight();
    double scale = Math.min(xscale, yscale);
    t2d.scale(scale, scale);
    try {
      g2d.drawImage(bImage,t2d, this);
    }
    catch (Exception ex) {
      ex.printStackTrace();
      return Printable.NO_SUCH_PAGE;
    }
    return Printable.PAGE_EXISTS;
  }

  void print() {
    PrinterJob printJob = PrinterJob.getPrinterJob();
    PageFormat pageFormat = printJob.defaultPage();
    pageFormat.setOrientation(PageFormat.LANDSCAPE);
    pageFormat = printJob.validatePage(pageFormat);
    printJob.setPrintable(this, pageFormat);
    if (printJob.printDialog()) {
      try {
        printJob.print();
      }
      catch (PrinterException ex) {
        ex.printStackTrace();
      }
    }
  }

  public boolean imageUpdate(Image img,
    int infoflags,
    int x,
    int y,
    int width,
    int height) {
    return false;
  }

  ImagePrinter(BufferedImage bImage) {
    this.bImage = bImage;
  }
}


class ImageDisplayer extends JFrame implements ActionListener {
  BufferedImage bImage;

  private class ImagePanel extends JPanel {
    public void paint(Graphics g) {
      g.setColor(Color.black);
      g.fillRect(0, 0, getSize().width, getSize().height);
      g.drawImage(bImage, 0, 0, this);
    }

    private ImagePanel() {
      setPreferredSize(new Dimension(bImage.getWidth(),
        bImage.getHeight()));
    }
  }

  private JMenuItem printItem;
  private JMenuItem closeItem;

  public void actionPerformed (ActionEvent event) {
    Object target = event.getSource();

    if (target == printItem) {
      new ImagePrinter(bImage).print();
    }
    else if (target == closeItem) {
      this.removeAll();
      this.setVisible(false);
      bImage = null;
    }
  }

  private JMenuBar createMenuBar() {
    JMenuBar menuBar = new JMenuBar();
    JMenu fileMenu = new JMenu("File");
    printItem = new JMenuItem("Print...");
    printItem.addActionListener(this);
    closeItem = new JMenuItem("Close");
    closeItem.addActionListener(this);
    fileMenu.add(printItem);
    fileMenu.add(new JSeparator());
    fileMenu.add(closeItem);
    menuBar.add(fileMenu);
    return menuBar;
  }

  ImageDisplayer(BufferedImage bImage) {
    this.bImage = bImage;
    this.setTitle("Off-screen Canvas3D Snapshot");

    // Create and initialize menu bar
    this.setJMenuBar(createMenuBar());

    // Create scroll pane, and embedded image panel
    ImagePanel imagePanel = new ImagePanel();
    JScrollPane scrollPane = new JScrollPane(imagePanel);
    scrollPane.getViewport().setPreferredSize(new Dimension(700, 700));

    // Add scroll pane to the frame and make it visible
    this.getContentPane().add(scrollPane);
    this.pack();
    this.setVisible(true);
  }
}

package jgui;
import javax.swing.*;
import java.awt.*;
import prolog.logic.*;

public class JinniText extends Displayer {
	public JinniText() {
		this("");
	}
    
	public static int PIXS=30; ////
	
	public JinniText(String oldText) {
		super(Start.defCols*PIXS,Start.defRows*PIXS,"text/plain");
		set_text(oldText);
		validate();
	}

	public void setRows(int rows) {
		// TODO
		//Prolog.dump("setRows="+rows);
        Dimension D=this.getSize();
		setDims(D.width,PIXS*rows);
	}

	public void setColumns(int cols) {
		// TODO
		//Prolog.dump("setCols="+cols);
        Dimension D=this.getSize();
		setDims(PIXS*cols,D.height);
	}
	public String getText() {
		return super.get_text();
	}

	public void setText(String s) { // set_text in Prolog Start.setLooks(this);
		super.set_text(s);
	}
}


package agentgui;

public class Main {
  public static VFrame vframe;
  
  synchronized public static InnerFrame new_inner_frame(String title) {
	InnerFrame iframe=new InnerFrame(title);
	vframe.addInnerFrame(iframe);
	return iframe;
  }
  
  public static void startgui() {
	VFrame.startGUI(new VFrame());
  }
  
  public static void main(String[] args) {
	  startgui();
  }
}



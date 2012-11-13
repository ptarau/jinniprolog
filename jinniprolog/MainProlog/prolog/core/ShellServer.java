package prolog.core;
import prolog.kernel.*;
import prolog.logic.*;
import java.net.*;

/**
Allows ShellClient components to connect from remote machines and
run commands on a Prolog server.
 */
public class ShellServer extends Transport implements Runnable {
  public ShellServer(String args[], int port) throws SystemException {
    super(port);
    this.args = args;
  }

  public ShellServer(int port) throws SystemException {
    this(null, port);
  }

  private String[] args;

  public void run() {
    for (; ; ) {
      Transport service = new Transport(this);
      PrologReader R;
      PrologWriter W;
      try {
        Socket socket = service.client_socket;
        R = JavaIO.toReader(socket.getInputStream());
        W = JavaIO.toWriter(socket.getOutputStream());
      }
      catch (Exception e) {
        JavaIO.errmes("error in ShellServer", e);
        break;
      }

      Shell S = new Shell(args, R, W, false, "?-| ");
      Thread T = new Thread(S,"ShellThread"); T.setDaemon(true);
      T.start();
    }
  }
}
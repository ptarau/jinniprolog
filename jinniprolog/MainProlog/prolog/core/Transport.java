package prolog.core;
import prolog.kernel.*;
import prolog.logic.*;

import java.net.*;
import java.io.*;
import java.util.*;
import java.util.zip.*;

/**
 * Transport layer: provides socket based client/server interaction
 * Supports various term encodings - from plain ascii to serialized and encrypted
 * Called though Reflection from Prolog.
 */
public class Transport implements Stateful {

  /**
   * Client
   */
  public Transport(String host, int port, int try_connect) throws SystemException {
    this.host = host;
    this.port = port;
    if (try_connect > 0 && !connect()) {
      throw new SystemException("Socket Error: client creation failure on host=" + host + " port=" + port);
    }
  }

  public Transport(String host, int port) throws SystemException {
    this(host, port, 1);
  }

  /**
   * Server constructor
   */
  public Transport(int port) throws SystemException {
    this.host = null;
    this.port = port;
    try {
      server_socket = new ServerSocket(port);
    }
    catch (IOException e) { server_socket = null; }

    if (null == server_socket)
      JavaIO.errmes("Socket Error: server creation failure: " + port,
                     new SystemException()
                   );
    else {
      JavaIO.println("Server listening on port: " + port);
    }
  }

  public static Transport newService(Transport Server) {
    Transport service = new Transport(Server);
    if (service.server_socket == null) service = null;
    else JavaIO.traceln("Server starting services.");
    return service;
  }

  /**
   * Service constructor
   */
  public Transport(Transport T) {
    this.server_socket = T.server_socket;
    if (accept()) return;
    this.server_socket = null;
    JavaIO.errmes("Socket Error: service creation failure on: " + T.server_socket, null);
  }

  public String host;
  public int port;
  transient protected DataInputStream from;
  transient protected DataOutputStream to;
  transient protected Socket client_socket;
  transient protected ServerSocket server_socket;


  static final int OTypeTag = -1; // can be any negative number - more tags can be used

  /**
    Reads from a socket stream. The data format is compatible with BinProlog's C base
    data format <int length,bytes> while allowing for encrypted or unenecrypted serialized
    objects as a transport mechanism.
  */
  synchronized public Object read_from() {
    Object o = null;
    if (null == from) {
      JavaIO.warnmes("read_from: null socket exception");
      return o;
    }
    try {
      //Prolog.dump("read_from TRACE available bytes:"+"<"+from.available()+"> on "+client_socket);
      int l = from.readInt();
      if (l >= 0)
        o = sread_from(from, l);
      else if (OTypeTag == l)
        o = oread_from(from);
      else {
        JavaIO.warnmes("Bad int data type in read_from: " + l);
      }
    }
    catch (EOFException e) {
      //JavaIO.warnmes("EOF Exception in read_from: " + e);
    }
    catch (IOException e) {
      JavaIO.warnmes("IO Exception in read_from: " + e);
    }
    catch (ClassNotFoundException e) {
      JavaIO.warnmes("ClassNotFoundException in read_from: " + e);
    }
    /*catch (Throwable e) {
       JavaIO.errmes("UNEXPECTED ERROR IN read_from()",e);
    }*/
    JavaIO.traceln("read_from TRACE:" + "<" + o + ">");
    if (null == o) disconnect();
    return o;
  }

  /**
    Writes to a socket stream.
  */
  synchronized public void write_to(Object o) {
    try {
      if (o instanceof String)
        swrite_to(to, (String)o);
      else
        owrite_to(to, o);
    }
    catch (IOException e) {
      JavaIO.warnmes("IO Exception in write_to: " + e + "<==" + o);
      disconnect();
    }
    JavaIO.traceln("write_to TRACE:" + "<" + o + ">");
  }

  static final private String sread_from(DataInputStream f, int length) throws IOException {
    byte bs[] = bread_from(f, length);
    String s = new String(bs);
    return s;
  }

  static final private void swrite_to(DataOutputStream f, String s) throws IOException {
    byte[] bs = s.getBytes();
    f.writeInt(bs.length);
    bwrite_to(f, bs);
  }

  public static String force_utf8(String s) {
    return force_encoding(s,"ISO-8859-1","UTF-8");
  }
  
  public static String force_encoding(String s,String fromFormat,String toFormat) {
    try {
      byte[] bs=s.getBytes(fromFormat);
      s=new String(bs,toFormat);
    }
    catch(UnsupportedEncodingException e) {
    }
    return s;
  }
      
  public static byte[] file2bytes(String fname) throws IOException {
    File file = new File(fname);
    int l = (int)(file.length());
    DataInputStream f = new DataInputStream(new FileInputStream(file));
    byte[] bs = bread_from(f, l);
    f.close();
    return bs;
  }

  public static final byte[] bread_from(DataInputStream f, int length) throws IOException {
    byte bs[] = new byte[length];
    f.readFully(bs);
    return bs;
  }

  public static final void bwrite_to(OutputStream f, byte[] bs) throws IOException {
    f.write(bs);
    f.flush();
  }

  /**
   *  Reads a serialized object
   */
  static final private Object oread_from(DataInputStream f) throws IOException,ClassNotFoundException {
    Object o = (new ObjectInputStream(f)).readObject();
    //Prolog.dump("READ OBJECT: <"+o+">:"+o.getClass());
    return o;
  }

  /**
   * Writes a serialized object
   */
  static final private void owrite_to(DataOutputStream f, Object o) throws IOException {
    f.writeInt(OTypeTag);
    f.flush();
    ObjectOutputStream g = new ObjectOutputStream(f);
    //Prolog.dump("WRITE OBJECT: <"+o+">:"+o.getClass()); 
    g.writeObject(o);
    g.flush();
  }

  static public boolean compress = true;

  static public void compressOn() {
    compress = true;
  }

  static public void compressOff() {
    compress = false;
  }

  static public boolean toFile(String fname, Object O) {
    try {
      ObjectOutputStream f =
      (compress) ? new ObjectOutputStream(new DeflaterOutputStream(new FileOutputStream(fname)))
                : new ObjectOutputStream(new FileOutputStream(fname));
      f.writeObject(O);
      f.close();
      return true;
    }
    catch (IOException e) {
      JavaIO.errmes("unable to save object to file: " + fname, e);
      return false;
    }
  }

  static public Object fromFile(String fname) {
    try {
      ObjectInputStream f =
        (compress) ? new ObjectInputStream(new InflaterInputStream(new FileInputStream(fname)))
                  : new ObjectInputStream(new FileInputStream(fname));
      Object O = f.readObject();
      f.close();
      return O;
    }
    catch (Exception e) {
      JavaIO.errmes("unable to rebuild object from file: " + fname, e);
      return null;
    }
  }

  static protected void traceln(String s) {
    JavaIO.traceln(s);
  }

  /**
     Opens read/write streams of a Connector socket
  */
  protected void open_streams() throws IOException {
    from = new DataInputStream(new BufferedInputStream(
              client_socket.getInputStream()));
    to = new DataOutputStream(new BufferedOutputStream(
              client_socket.getOutputStream()));
  }

  /** 
  * disconects a client or service socket
  */
  public void disconnect() {
    if (null != client_socket) {
      try {
        //Prolog.dump("disconnecting client_socket: "+client_socket);
        traceln("disconnecting client_socket: " + client_socket);
        client_socket.close();
        from = null;
        to = null;
        client_socket = null;
      }
      catch (IOException e) {
        JavaIO.errmes("disconnect: failing to close socket", e);
      }
    }
  }


  /** 
   * discontinues future services on this server_socket
   */
  public void discontinue() {
    //Prolog.dump("disconnecting server_socket: "+server_socket);
    if (null != server_socket) {
      try {
        traceln("disconnecting server_socket: " + server_socket);
        server_socket.close();
        from = null;
        to = null;
        server_socket = null;
      }
      catch (IOException e) {
        JavaIO.errmes("disconnect: failing to close server socket", e);
      }
        }
    }

  /**
    Creates a Client socket. Note that it spends some
    time retrying to connect to a Server.
  */
  static int SRETRY = 10;

  private boolean create_socket() {
    traceln("Creating socket to host=" + host + " port=" + port);
    boolean ok = false;
    try {
      for (int i = 0; i < SRETRY; i++) {
        try {
          client_socket = new Socket(host, port);
          client_socket.setTcpNoDelay(true);
          //client_socket.setSoLinger(false,0);
          break;
        }
        catch (BindException e) {
          // workaround to OS bug (on various Windows)
          long waitingTime = 1000L * (1 << i);
          Prolog.dump("waiting " + waitingTime +
                        "ms for broken Operating System to close old sockets:" + e);
          try {
            Thread.sleep(waitingTime);
          }
          catch (InterruptedException ie) {
          }
        }
      }
      ok = true;
    }
    catch (UnknownHostException e) {
      //JavaIO.errmes("create_socket: bad host="+host+" or port="+port,e);
    }
    catch (IOException e) {
      //JavaIO.errmes("error in create_socket",e);
    }
    if (ok)
      traceln("created client_socket: " + client_socket.toString());
    else
      traceln("client: socket creation failure");
        return ok;
     }

  /**
    Opens the read/write streams of a Client socket
  */
  private boolean open_connection() {
    boolean ok = false;
    try {
      open_streams();
      ok = true;
    }
    catch (IOException e) { }
    if (ok) {
      traceln("client connected!");
    }
    else JavaIO.warnmes(
      "client connection failure"
    );
    return ok;
  }

  public boolean connect() {
    boolean ok = create_socket() && open_connection();
    if (!ok) disconnect();
    return ok;
  }

  private boolean accept() {
    try {
      if (null == client_socket) {
        client_socket = server_socket.accept();
        open_streams();
      }
    }
    catch (IOException e) {
      //JavaIO.warnmes("error accepting on server socket:"+server_socket);
      disconnect();
      return false;
    }
    return true;
  }

  
  
  
  /* new server API */
  
  public static boolean server(int port) {
     return server(port,null);
  }
  
  public static boolean server(int port,String bpFile) {
    try {
      Transport server=new Transport(port);
      Machine I=Top.newProlog((String)null);
      for(;;) {
        if(!server.serverStep(I)) return false;
      }
    }
    catch(Exception e) {
      Interact.errmes("error in server on port: "+port);
      return false;
    }
  }
  
  synchronized private boolean serverStep(Machine I) {
     boolean ok=false;
     try {
       Transport service=newService(this);
       for(;;) {
         Object raw=service.read_from();
         if(null==raw) break;
         //Prolog.dump("RAW="+raw+":"+raw.getClass());
         Object[] in=(Object[])raw;
        
         Object[] out=Top.askProlog(I,in);
         //Prolog.dump("SERVER COMPUTED: "+out+ "=>"+out.getClass());
         service.write_to(out);
         ok=true;
       }
     }
     catch(Exception e) {
       e.printStackTrace();
     }
     return ok;
  }
  
  public static Transport connection(String host,int port) {
    try {
      return new Transport(host,port);
    }
    catch(Exception e) {
      Interact.errmes("error in creating client on host="+host+",port="+port);
      return null;
    }
  }
  
  synchronized public static Object ask_lserver(Transport service,Machine E) {
    try {
      Object[] bundle=E.getBundle();
      service.write_to(bundle);
      Object in=service.read_from();
      //Prolog.dump("CLIENT GOT: "+in+ "=>"+in.getClass());
      bundle=(Object[])in;
      E.setBundle(bundle);
      return "yes";
    }
    catch(Exception e) {
      Interact.errmes("error in ask_server on host="+service.host+",port="+service.port);
      return "no";
    }
  }
   
}


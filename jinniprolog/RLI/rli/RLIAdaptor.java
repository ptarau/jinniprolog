package rli;

import prolog.logic.Fun;
import prolog.logic.Var;
import prolog.logic.Prolog;
import prolog.kernel.PrologReader;
import prolog.kernel.PrologWriter;
import java.net.NetworkInterface;
import java.net.InetAddress;
import java.util.*;

public class RLIAdaptor {

  public static boolean trace=false;

  public static boolean cacheServers=false;
  
  public static Map InnerServers = Collections.synchronizedMap(new HashMap());
  
  /**
   * Independent testing function
   */
  public static void main(String args[]) {
    String host=null; // means "localhost";
    String portName="7777"; // ll ports are virtual - their names are strings -
    // avoid confusion with socket layers
    if("server".equals(args[0])) {
      if(rli_start_server(portName,null)<0)
        System.exit(1);
    } else {
      Fun goal=new Fun("println","hello");
      Object answer=rli_call(host,portName,goal);
      System.out.println("got: "+answer);
    }
  }

  public static Object Stopper=new Object();

  // basic named port API

  //
  public static int rli_start_server(String portName,Prolog prolog,
      PrologReader reader,PrologWriter writer) {
    RLIServer server=new RLIServer(portName,prolog,reader,writer);
    int r=server.start(); // first !!!
    if(r>=0) InnerServers.put(portName,server);
    return r;
  }

  public static int rli_start_server(String portName,Prolog prolog,
      PrologWriter writer) {
    return rli_start_server(portName,prolog,null,writer);
  }

  public static int rli_start_server(String portName,Prolog prolog) {
    return rli_start_server(portName,prolog,null,null);
  }

  public static void rli_stop_server(String host,String portName) {
    RLIClient.stopServer(host,portName);
    if("localhost".equals(host)) InnerServers.remove(portName);
  }

  //
  public static Object rli_call(String host,String portName,Object goal) {
    if(cacheServers && "localhost".equals(host)) {
      RLIServer inner=(RLIServer)InnerServers.get(portName);
      if(null!=inner) return inner.serverCallProlog(goal); //?? bg
    }
    return RLIClient.clientPrologCall(host,portName,goal);
  }

  public static int rli_wait(String host,String portName,int timeout) {
    return RLIClient.rli_wait(host,portName,timeout);
  }

  
  public static Object rli_in(String host,String port) {
    if(cacheServers && "localhost".equals(host)) {
      RLIServer inner=(RLIServer)InnerServers.get(port);
      if(null!=inner) return inner.rli_in(); //?? bg
    }
    return RLIClient.rli_in(host,port);
  }

  public static void rli_out(String host,String port,Object T) {
    if(cacheServers && "localhost".equals(host)) {
      RLIServer inner=(RLIServer)InnerServers.get(port);
      if(null!=inner) {
        inner.rli_out(T); //?? bg
        return;
      }
    }
    RLIClient.rli_out(host,port,T);
  }

  // derived int port API - for backward compatilility

  public static int rli_start_server(int port,Prolog prolog) {
    return rli_start_server(""+port,prolog);
  }

  public static void rli_stop_server(String host,int port) {
    rli_stop_server(host,""+port);
  }

  public static Object rli_call(String host,int port,Object goal) {
    return rli_call(host,""+port,goal);
  }

  public static int rli_wait(String host,int port,int timeout) {
    return rli_wait(host,""+port,timeout);
  }
  
  public static Fun rli_get_inets() {
    Vector V=new Vector();
    try {
      Enumeration E=NetworkInterface.getNetworkInterfaces();
      while(E.hasMoreElements()) {
        NetworkInterface I=(NetworkInterface)E.nextElement();
        Enumeration A=I.getInetAddresses();
        while(A.hasMoreElements()) {
          InetAddress IA=(InetAddress)A.nextElement();
          if(IA.isSiteLocalAddress()) {// || IA.isLoopbackAddress()) {
            String adr=IA.getHostAddress();
            // String adr=IA.getCanonicalHostName();
            // String adr=IA.toString();
            V.addElement(adr);
          }
        }
      }
      // System.err.println("!!!V="+V);
      Object[] args=V.toArray();
      return new Fun("inetAddresses",args);
    } catch(Exception e) {
      e.printStackTrace();
      return null;
    }
  }

  /*
   * // new $$ public static RLIServer new_rserver(String portName,Prolog
   * prolog) { RLIServer rserver=new RLIServer(portName,prolog);
   * //rserver.start(); return rserver; } // new $$ public static Object
   * rserver_call(RLIServer rserver,Object goal) { return
   * rserver.serverCallProlog(goal); }
   */
}

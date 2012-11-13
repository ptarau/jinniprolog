package prolog.core;
import prolog.kernel.*;
import prolog.logic.*;

import java.util.*;
import java.net.*;
import java.io.*;

/**
 * Implements basic HTTP protocol allowing Prolog to act like a
 * a self contained Web service - in particular to serve it's
 * own code and Prolog files over the Web for supporting a Prolog applet.
 * 
 * use: 
 *  run_http_server -- on default Port 8001
 *  run_http_server(Port)    
 */
public class HttpService implements Runnable, Stateful
{
  transient private Socket serviceSocket;
  private String www_root;

  public HttpService(Socket serviceSocket,String www_root)
  {
    this.serviceSocket=serviceSocket;
    this.www_root=www_root;
  }

  byte[] fileORjarfile2bytes(String fname) throws IOException {
    //Prolog.dump("trying: "+fname);
    try 
    {
      return Transport.file2bytes(fname);
    }
    catch(IOException e) {  
      //throw e;
      if(fname.startsWith("./")) fname=fname.substring(2);
      InputStream in=Zipper.zip2stream(Top.ZIPSTORE,fname,true);
      if(null==in) throw e;
      BufferedInputStream stream=new BufferedInputStream(in);
      //Prolog.dump("zipstore: "+fname);
      try 
      {
        return streamToBytes(stream); 
      }
      catch(ExistenceException ee) 
      {
        throw e;
      }
    }
  }

  public static byte[] streamToBytes(InputStream in) throws ExistenceException {
    if(null==in) return null;
    try {
      IntStack is=new IntStack();
      for(;;) {
        int c=in.read();
        if(c==-1) break;
        is.push((byte)c);
       }
      in.close();
      return is.toByteArray();
    }
    catch(IOException e) {
        throw new ExistenceException("error in zip file");
    }
  }
   
  public static String content_header="HTTP/1.0 200 OK\nContent-Length: ";
  
  private final String fix_file_name(String f) {
    return 
      www_root.concat(
      (f.endsWith("/")
      ? f.concat("index.html")
      : f)
      );
  }
  
  static private final String nextToken(String s) {
    StringTokenizer t=new StringTokenizer(s," ");
    t.nextToken();
    return t.nextToken();
  }
  
  public void run() 
  {
    try
    {
      DataInputStream in=new DataInputStream(serviceSocket.getInputStream());
      DataOutputStream out=new DataOutputStream(serviceSocket.getOutputStream());
      try 
      {
        while(true)
        {
          String s=in.readLine();
          if(s.length()<1) break;
          if(s.startsWith("GET"))
          {
            //Prolog.dump(s);
            String f=nextToken(s);
            f=fix_file_name(f);
            byte[] bs=fileORjarfile2bytes(f);
            //Prolog.dump("cl="+bs.length);
            out.writeBytes(content_header+bs.length+"\n\n");
            Transport.bwrite_to(out,bs);
          }
          if(s.startsWith("POST")) {
            //Prolog.dump(s);
            String f=nextToken(s);
            f=fix_file_name(f);
            String line="?";
            String contentLength=null;
            String userAgent=null;
            while(!line.equals("")) {
                line=in.readLine().toLowerCase();
                if(line.startsWith("content-length:"))
                  contentLength=nextToken(line);
                else if(line.startsWith("user-agent:"))
                  userAgent=nextToken(line);   
            }
           
            int content_length=Integer.parseInt(contentLength);
            //Prolog.dump("cl="+content_length);
            
            byte[] is=new byte[content_length];
            in.readFully(is);
            //Prolog.dump("alist="+new String(is));            
          
            String result=call_jinni_post_handler(new String(is),f);
            byte[] os;
            if(null==result || result==f) {
              // use template file
              os=fileORjarfile2bytes(f);
            }
            else {
              // use result - it assumes the client 
              // processed the template file + action script
              os=result.getBytes();
            }
            out.writeBytes(content_header+os.length+"\n\n");
            Transport.bwrite_to(out,os);
          }
          else /* ignore other headers */
          {
            // Prolog.dump(s);
          }
          
        }
      } 
      catch(Exception e) 
      {
        out.writeBytes("HTTP/1.0 404 ERROR\n\n\n");
      }
      in.close();
      out.close();
    }
    catch(SocketException se) 
    {
      // ok- just trying to write - when client closed first
    }
    catch(Exception ee)
    {
      JavaIO.errmes("http_service_error",ee);
    }
  }

  private final String call_jinni_post_handler(String is,String os) {
    Machine M=Top.new_machine(null,null);
    if(null==M) return null;
    Object R=new Var(1);
    Fun Goal=new Fun("post_method_wrapper",is,os,R);
    Fun Query=new Fun(":-",R,Goal);
    if(!M.load_engine(Query)) return null;
    try {
      // to allow gc of objects involved the Prolog query should not fail !!!
      String answer=(String)M.get_answer();
      M.removeObject(is);
      M.removeObject(os);
      M.removeObject(answer);
      M.stop();
      return answer;
    }
    catch(Exception e) {
      //Prolog.dump("exception in POST method handler"+e);
      return null;
    }
  }
  
  /**
   * Starts a HTTP server rooted in the directory www_root
   */
  public static void run_http_server(int port,String www_root) 
  {
    try 
    {
      ServerSocket serverSocket=new ServerSocket(port);
      while(true) 
      {
        Socket serviceSocket=serverSocket.accept();
        Thread T=
          new Thread(new HttpService(serviceSocket,www_root),"HttpThread");
        T.setDaemon(true);
        T.start();
      }
    }
    catch(Exception e)
    {
      JavaIO.errmes("http_server_error",e);
    }
  }
}

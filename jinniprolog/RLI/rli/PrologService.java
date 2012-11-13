package rli;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface PrologService extends Remote {
  public Object serverCallProlog(Object goal) throws RemoteException;
  public Object rli_in() throws RemoteException;
  public void rli_out(Object T) throws RemoteException;
}

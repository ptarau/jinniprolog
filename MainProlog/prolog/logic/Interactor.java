package prolog.logic;

/**
 * Abstract class describing a "mixed initiative" Interactor
 * that is controlled by a client coroutine but it has a say
 * on when values are returned. It can also receive
 * additional data from parent if it agrees to. Note that
 * communication is purely cooperative and therefore this
 * is significantly more efficient than using Threads.
 */
public abstract class Interactor implements Stateful {

  /** 
   * called by the client to initialize this Interactor with data
   */
  public abstract Interactor init(Object data); // new_engine

  /**
   * called by the client to reinitialize this Interactor with new data
   */
  public abstract void reuse(Object data);


  /**
   * called by client to free all resources and stop all processing
   */
  public abstract void stop(); // stop

  /* 
   * called by client to initiate a processing step that results
   * in a value made available by this Interactor issuing a yield() 
   * operation, or a null value indicating that no more values can
   * be produced
  */
  public abstract Object client_get(); // get
  // in a MT context this can start speculatively
  // to work on the computations associated with
  // the following next(), provided that data
  // exchanges are absent or coordinated with parent                

  /**
   * called by client to provide new data to this Interactor
   */
  public abstract void client_send(Object what); // to_engine
  
  /* 
   * called by this Interactor to save its own state and produce
   * a return value available to the client issuing a 
   * client_get() operation
   */
  public abstract Object yield_return(); // return

  /**
   * called by this Interactor to receive new data from parent
   */
  public abstract Object receive(); // from_engine
}

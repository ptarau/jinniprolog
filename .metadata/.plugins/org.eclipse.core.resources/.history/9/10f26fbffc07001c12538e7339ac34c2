package prolog.core;
import prolog.kernel.*;
import prolog.logic.*;

public class RankedData implements Stateful {
  public RankedData(Object data) {
    this.data = data;
    clear();
  }

  public void clear() {
    this.rank = 0.0;
    this.hyper = 0;
    this.component = -1;
    this.color = 0;
  }

  public final Object data;
  public double rank;
  public int component;
  public int hyper;
  public int color;

  public String toString() {
    String s = "";
    if (null != data) s = data.toString();
    return "<" + s + ">:(r=" + Machine.double2string(rank, 2) + ",h=" + hyper + ",c=" + component + ",x=" + color + ")";
  }
}

package prolog3d;

public class DataPoint {
  DataPoint() {
  }
  
  DataPoint(double x,double y,double z) {
    this.x=x;
    this.y=y;
    this.z=z;
  }
  
  double x=0;
  double y=0;
  double z=0;
  double dx=0;
  double dy=0;
  double dz=0;

  public String toString() {
    return "(dx="+dx+",dy="+dy+",dz="+dz+")";
  }
}

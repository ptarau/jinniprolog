package prolog3d;
import prolog.core.RankedData;

import java.text.NumberFormat;
import javax.vecmath.*;

class MobilePoint {
  MobilePoint() {
  }
  
  static double viewDistance=0;//1.2;
  
  MobilePoint(Object RS,RankedData RD,Vertex3D V,double r,double n) {
    this.r=r;
    if(null!=RD) {
      if(null!=RD.data && null!=RS) this.lbl=RS.toString();
      else this.lbl="";
      this.rank=RD.rank;
      this.component=RD.component;
    }
    else {
      this.lbl="??";
      this.rank=1.0;
      this.component=0;
    }
    this.V=V;
    double idealSize=Math.pow(rank,1/3f);
    idealSize=0.01*idealSize*(r/n);
    //Prolog3D.pp("idealSize="+idealSize);
    double max=0.5; double min=0.1;
    if(idealSize>max) idealSize=max;
    else if(idealSize<min) idealSize=min;
    V.scaleTo(idealSize);
    initZ();
  }
  
  Vertex3D V;
  
  double rank;
  int component;
  boolean fixed;
  String lbl;
  
  private double x;
  private double y;
  private double z;
  
  double dx;
  double dy;
  double dz;
   
  private double r;
  
  final double getX() {
    return this.x;
  }
  
  final double getY() {
    return this.y;
  }
  
  final double getZ() {
    return this.z;
  }
  
  final void setX(double x) {
    this.x=x;
    double rx=x/r;
    V.setX(rx);
  }
  
  final void setY(double y) {
    this.y=y;
    double ry=y/r;
    V.setY(ry);
  }
  
  final void setZ(double z) {
    this.z=z;
    double rz=z/r-viewDistance; //$ 
    V.setZ(rz);
  }
  
  final void initZ() {
    double rz;
    double t=Math.min(component,10)/10.0;
    rz=-t*r;
    V.setZ(rz);
    //Prolog3D.pp("rz="+rz);
  }
  
  final void incX(double x) {
    setX(getX()+x);
  }
  
  final void incY(double y) {
    setY(getY()+y);
  }
  
  final void incZ(double z) {
    setZ(getZ()+z);
  }
  
  public String toString() {
    // called through Vertex V's toString which is basically this
    if(rank==0.0 || Params.verbose<1) return lbl;
    NumberFormat nf = NumberFormat.getInstance();
    nf.setMaximumFractionDigits(2);
    String pr=nf.format(rank);
    return lbl+":"+pr+"("+component+")";
  }
}

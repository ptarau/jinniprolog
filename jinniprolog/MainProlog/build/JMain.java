package build;
import prolog.core.Javafier;
public class JMain extends Javafier {
 public static void main (String args[]) {
   if(args.length>0) 
     javafy(args[0]);
   else
     run();
 }
}
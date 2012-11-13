package prolog.core;
import prolog.kernel.*;
import java.io.IOException;
import prolog.logic.*;

public class CatXMLWriter implements CatWalker {
  CatXMLWriter(PrologWriter writer) {
    this.writer=writer;
  }
  
  CatXMLWriter(String fileName) throws IOException {
    this(new PrologWriter(fileName));
  }

  private PrologWriter writer;

  public void atStart() {
    writer.println("<?xml version=\"1.0\"?>");
    writer.println("<cat>");
  }

  public void beforeProps() {
    writer.println("  <objects>");
  }

  public void onProp(Object vertex,Object key,Object value) {
    writer.println("    <object>");
    writer.println("      <node>"+vertex+"</node>");
    writer.println("      <attribute>"+key+"</attribute>");
    writer.println("      <value>"+value+"</value>");
    writer.println("    </object>");
  }

  public void afterProps() {
    writer.println("  </objects>");
  }

  public void beforeMorphisms() {
    writer.println("  <morphisms>");
  }

  public void onMorphism(Object from,Object to,Object m,Object md){
    writer.println("    <morphism>");
    writer.println("      <from>"+from+"</from>");
    writer.println("      <to>"+to+"</to>");
    writer.println("      <attribute>"+m+"</attribute>");
    writer.println("      <value>"+m+"</value>");
    writer.println("    </morphism>");
  }

  public void afterMorphisms() {
    writer.println("  </morphisms>");
  }

  public Object atEnd() {
    writer.println("</cat>");
    writer.close();
    return null;
  }
}

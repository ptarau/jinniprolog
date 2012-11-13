package prolog.core;
import prolog.kernel.*;
import java.util.BitSet;
import prolog.logic.*;

/**
Implements efficient Role managment on a
given ObjectDict, assumed immutable. Roles
are implemented as BitSets to which the
dictionary based integer ordinal of a key
is used to turn on/off a corresponding key.
*/

public class RoleMap implements Stateful {
  final private ObjectDict map;
  final private BitSet roles;

  private static BitSet emptySet = new BitSet();

  /**
     Creates a RoleMap based on an external role
     dictionary which contain meaningful names
     for the rules represented as bits in a BitSet.
   */
  RoleMap(ObjectDict map) {
    this.map = map;
    this.roles = new BitSet();
  }

  /**
   Creates a RoleMap with a new empty Role dictionary.
   */
  RoleMap() {
    this(new ObjectDict());
  }

  /**
    Creates a RoleMap based on given dictionary and roles
   */
  RoleMap(ObjectDict map, BitSet roles) {
    this.map = map;
    this.roles = roles;
  }

  /**
    Creates a RoleMap based on another RoleMap's role dictionary.
   */
  RoleMap(RoleMap other) {
    this(other.map);
  }

  public Object clone() throws CloneNotSupportedException {
    return new RoleMap(map, (BitSet)(roles.clone()));
  }

  public BitSet getRoles() {
    return roles;
  }

  /**
    Adds a new role and associates an attribute to it.
   */
  public void add(Object key, Object val) {
    map.put(key, val);
  }

  /**  
     gets the attribute associated to a role
   */
  public Object getValue(Object key) {
    return map.get(key);
  }

  public void setValue(Object key, Object value) {
    Entry entry = map.getEntry(key);
    entry.value = value;
  }

  public void set(int bit) {
    roles.set(bit);
  }

  public boolean get(int bit) {
    return roles.get(bit);
  }

  public void clear(int bit) {
    roles.clear(bit);
  }

  public void clear() {
    roles.and(emptySet);
  }

  public void set(Object key) {
    roles.set(map.getOrdinal(key));
  }

  public boolean get(Object key) {
    return roles.get(map.getOrdinal(key));
  }

  public void clear(Object key) {
    roles.clear(map.getOrdinal(key));
  }

  public void and(RoleMap other) {
    roles.and(other.getRoles());
  }

  public void or(RoleMap other) {
    roles.or(other.getRoles());
  }

  public void xor(RoleMap other) {
    roles.xor(other.getRoles());
  }

  public void andNot(RoleMap other) {
    //return roles.clone().andNot(other.getRoles()); // only in 1.2
    BitSet oroles = (BitSet)other.getRoles().clone();
    oroles.xor(emptySet);
    roles.and(oroles);
  }

  public ObjectDict toDict() {
    ObjectIterator I = map.getKeys();
    ObjectDict D = new ObjectDict();
    while (I.hasNext()) {
      Object k = I.next();
      if (get(k)) D.put(k, getValue(k));
    }
    return D;
  }

  public String toString() {
    return toDict().toString();
  }
}

package prolog.logic;
import java.util.Random;

public final class Tools implements Stateful {

  private final static Random defRandom = new Random(13);

  public static final Random getRandom() { return defRandom; }

  public static final void sort(Object[] a) {
    Object os[] = (Object[])a.clone();
    msort(os, a, 0, a.length, 0);
  }

  private static final void msort(Object src[], Object dest[],
    int low, int high, int off) {
    int length = high - low;

    if (length < 7) {
      for (int i = low; i < high; i++)
        for (int j = i; j > low &&
          ((Comparable)dest[j - 1]).compareTo((Comparable)dest[j]) > 0; j--)
          swap(dest, j, j - 1);
      return;
    }

    int destLow = low;
    int destHigh = high;
    low += off;
    high += off;
    int mid = (low + high) >> 1;
    msort(dest, src, low, mid, -off);
    msort(dest, src, mid, high, -off);

    if (((Comparable)src[mid - 1]).compareTo((Comparable)src[mid]) <= 0) {
      System.arraycopy(src, low, dest, destLow, length);
      return;
    }

    for (int i = destLow, p = low, q = mid; i < destHigh; i++) {
      if (q >= high || p < mid && ((Comparable)src[p]).compareTo(src[q]) <= 0)
        dest[i] = src[p++];
      else
        dest[i] = src[q++];
    }
  }

  private static final void swap(Object x[], int a, int b) {
    Object t = x[a];
    x[a] = x[b];
    x[b] = t;
  }

  public static Random newRandom(int seed) {
    return new Random(12345678901123L ^ (long)seed);
  }

  public static final void shuffle(int seed, Object x[], int max) {
    Random R = newRandom(seed);
    for (int j = max - 1; j > 1; j--) {
      int i = R.nextInt(j);
      swap(x, i, j);
    }
  }

  public static final void shuffle(int seed, Object x[]) {
    shuffle(seed, x, x.length);
  }

  public static String getRandomSym(String root) {
    return root + Math.abs(defRandom.nextLong());
  }

  private static Random uidRandom = new Random();

  public static String uid() {
    long uid = System.currentTimeMillis();
    uidRandom.setSeed(uid);
    uid = Math.abs(uidRandom.nextLong());
    return getRandomSym("uid") + uid;
  }
}

package lengine.runtime;

import lengine.functions.LengineLambda1;

public class LengineMapKey implements LengineLambda1<Object, LengineMap> {
  private final String key;

  private LengineMapKey(String key) {
    this.key = key;
  }

  public String getKey() {
    return key;
  }

  public static LengineMapKey create(String key) {
    return new LengineMapKey(key);
  }

  @Override
  public String toString() {
    return ":" + key;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;

    LengineMapKey that = (LengineMapKey) o;

    return key.equals(that.key);
  }

  @Override
  public int hashCode() {
    return key.hashCode();
  }

  @Override
  public Object invoke(LengineMap map) {
    return map.get(this);
  }
}

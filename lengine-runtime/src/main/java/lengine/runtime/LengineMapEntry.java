package lengine.runtime;

import java.util.Map;

public class LengineMapEntry implements Map.Entry {
  private final Object key;
  private final Object value;

  private LengineMapEntry(Object key, Object value) {
    this.key = key;
    this.value = value;
  }

  public static LengineMapEntry create(Object key, Object value) {
    return new LengineMapEntry(key, value);
  }

  @Override
  public Object getKey() {
    return key;
  }

  @Override
  public Object getValue() {
    return value;
  }

  @Override
  public Object setValue(Object value) {
    throw new IllegalArgumentException("the operation is not allowed");
  }
}

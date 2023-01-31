package lengine.runtime;

import java.util.Map;

public class LengineMapEntry implements Map.Entry<LengineMapKey, Object> {
  private final LengineMapKey key;
  private final Object value;

  private LengineMapEntry(LengineMapKey key, Object value) {
    this.key = key;
    this.value = value;
  }

  public static LengineMapEntry create(LengineMapKey key, Object value) {
    return new LengineMapEntry(key, value);
  }

  @Override
  public LengineMapKey getKey() {
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

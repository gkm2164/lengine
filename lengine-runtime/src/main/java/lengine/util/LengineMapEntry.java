package lengine.util;

import java.util.Map;

import lengine.runtime.exceptions.LengineTypeMismatchException;

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

  public static LengineMapEntry create(Map.Entry<LengineMapKey, Object> entry) {
    return new LengineMapEntry(entry.getKey(), entry.getValue());
  }

  public static LengineMapEntry cast(Object elem) {
    if (!(elem instanceof LengineMapEntry)) {
      throw new LengineTypeMismatchException(elem, LengineMapEntry.class);
    }
    return (LengineMapEntry) elem;
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

  @Override
  public String toString() {
    return "(entry " + key + " -> " + value + ")";
  }
}

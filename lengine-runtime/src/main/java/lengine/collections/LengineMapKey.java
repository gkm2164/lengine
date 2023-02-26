package lengine.collections;

import lengine.functions.LengineLambda1;
import lengine.collections.traits.LengineObjectType;
import lengine.runtime.LengineString;

import java.util.Optional;

public class LengineMapKey implements LengineLambda1<Object, LengineObjectType> {
  private final LengineString key;

  private LengineMapKey(LengineString key) {
    this.key = key;
  }

  public LengineString getKey() {
    return key;
  }

  public static LengineMapKey create(LengineString key) {
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
  public Object invoke(LengineObjectType map) {
    return Optional
            .ofNullable(map.get(this))
            .orElseGet(Nil::get);
  }
}

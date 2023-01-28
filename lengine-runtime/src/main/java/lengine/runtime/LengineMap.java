package lengine.runtime;

import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

public class LengineMap {
  private final Map<Object, Object> dictionary;
  private LengineMap() {
    this.dictionary = new HashMap<>();
  }

  public void put(Object key, Object value) {
    dictionary.put(key, value);
  }

  public Object get(Object key) {
    return dictionary.get(key);
  }

  public static LengineMap create() {
    return new LengineMap();
  }

  @Override
  public String toString() {
    return dictionary.entrySet().stream().map(entry -> {
      Object key = entry.getKey();
      Object value = entry.getValue();
      return String.format("%s => %s", key, value);
    }).collect(Collectors.joining(", ", "{", "}"));
  }
}

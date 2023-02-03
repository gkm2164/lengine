package lengine.runtime;

import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class LengineMap {
  private final LengineMap delegateMap;
  private final Map<LengineMapKey, Object> dictionary;
  private LengineMap() {
    this(null);
  }

  private LengineMap(LengineMap delegateMap) {
    this.delegateMap = delegateMap;
    this.dictionary = new HashMap<>();
  }

  public void putEntry(LengineMapEntry entry) {
    dictionary.put(entry.getKey(), entry.getValue());
  }

  public void put(LengineMapKey key, Object value) {
    dictionary.put(key, value);
  }

  public Object get(LengineMapKey key) {
    if (delegateMap != null && delegateMap.contains(key)) {
      return delegateMap.get(key);
    }
    return dictionary.get(key);
  }

  private boolean contains(LengineMapKey key) {
    return dictionary.containsKey(key);
  }

  public LengineList keys() {
    return LengineList.create(dictionary.keySet());
  }

  public LengineList entries() {
    return LengineList.create(dictionary.entrySet());
  }

  public LengineMap add(LengineMapEntry entry) {
    LengineMap map = new LengineMap(this);
    map.putEntry(entry);
    return map;
  }

  public static LengineMap create() {
    return new LengineMap();
  }

  public static LengineMap create(LengineMap delegateMap) {
    return new LengineMap(delegateMap);
  }

  public Stream<String> createStringEntry() {
    final Stream<String> parent;
    if (delegateMap != null) {
      parent = delegateMap.createStringEntry();
    } else {
      parent = Stream.empty();
    }

    return Stream.concat(parent, dictionary.entrySet().stream().map(entry -> {
      Object key = entry.getKey();
      Object value = entry.getValue();
      return String.format("%s => %s", key, value);
    }));
  }

  @Override
  public String toString() {
    return this.createStringEntry().collect(Collectors.joining(", ", "{", "}"));
  }
}

package lengine.runtime;

import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

import scala.collection.Seq;

public class LengineMap {
  private final Map<Object, Object> dictionary;
  private LengineMap() {
    this.dictionary = new HashMap<>();
  }

  public void putEntry(LengineMapEntry entry) {
    dictionary.put(entry.getKey(), entry.getValue());
  }

  public void put(Object key, Object value) {
    dictionary.put(key, value);
  }

  public Object get(Object key) {
    return dictionary.get(key);
  }

  public Sequence keys() {
    Sequence ret = new Sequence();
    dictionary.keySet().forEach(ret::add);
    return ret;
  }

  public Sequence entries() {
    Sequence ret = new Sequence();
    dictionary.forEach((key, value) -> ret.add(LengineMapEntry.create(key, value)));
    return ret;
  }

  public LengineMap add(LengineMapEntry entry) {
    LengineMap map = new LengineMap();
    Sequence sequence = entries();
    sequence.add(entry);
    sequence.iterator()
        .forEachRemaining(e ->
            map.putEntry((LengineMapEntry)e));
    return map;
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

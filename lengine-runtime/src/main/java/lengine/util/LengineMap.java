package lengine.util;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class LengineMap {
  private final Map<LengineMapKey, Object> dictionary;

  private LengineMap() {
    this.dictionary = new HashMap<>();
  }

  public void putEntry(LengineMapEntry entry) {
    dictionary.put(entry.getKey(), entry.getValue());
  }

  public void put(LengineMapKey key, Object value) {
    dictionary.put(key, value);
  }

  public Object get(LengineMapKey key) {
    return dictionary.get(key);
  }

  private boolean contains(LengineMapKey key) {
    return dictionary.containsKey(key);
  }

  public LengineList keys() {
    return LengineList.create(dictionary.keySet());
  }

  public LengineSequence entries() {
    LengineSequence sequence = LengineSequence.create(Nil.get());
    dictionary.entrySet()
        .stream()
        .map(entry -> LengineMapEntry.create(entry.getKey(), entry.getValue()))
        .forEach(sequence::add);

    return sequence;
  }

  public LengineMap add(LengineMapEntry entry) {
    LengineMap map = new LengineMap();
    dictionary.entrySet().stream().map(LengineMapEntry::create).forEach(map::putEntry);
    map.putEntry(entry);
    return map;
  }

  public static LengineMap create() {
    return new LengineMap();
  }


  public Stream<String> createStringEntry() {
    return dictionary.entrySet()
        .stream()
        .map(entry -> {
          Object key = entry.getKey();
          Object value = entry.getValue();
          return String.format("%s => %s", key, value);
        });
  }


  @Override
  public String toString() {
    return this.createStringEntry().collect(Collectors.joining(", ", "{", "}"));
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;

    LengineMap that = (LengineMap) o;
    return Objects.equals(dictionary, that.dictionary);
  }

  @Override
  public int hashCode() {
    return dictionary.hashCode();
  }
}

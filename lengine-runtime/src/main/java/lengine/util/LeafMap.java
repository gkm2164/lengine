package lengine.util;

import lengine.runtime.CreateIterator;
import lengine.runtime.LengineIterator;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class LeafMap extends LengineMap {
  private final Map<LengineMapKey, Object> dictionary;

  LeafMap() {
    super(false);
    this.dictionary = new HashMap<>();
  }

  LeafMap(CreateIterator it) {
    this();
    it.iterator().forEachRemaining(entry -> {
      LengineMapEntry e = (LengineMapEntry) entry;
      dictionary.put(e.getKey(), e.getValue());
    });
  }

  LeafMap(LengineMapEntry... entries) {
    this();
    for (LengineMapEntry entry : entries) {
      dictionary.put(entry.getKey(), entry.getValue());
    }
  }

  LeafMap(List<LengineMapEntry> list) {
    this();
    list.forEach(entry -> dictionary.put(entry.getKey(), entry.getValue()));
  }

  public LengineMap putEntry(LengineMapEntry entry) {
    if (lately) {
      return new NonLeafMap(true, this, new LeafMap(entry));
    } else {
      return new NonLeafMap(false, new LeafMap(entry), this);
    }
  }

  public Object get(LengineMapKey key) {
    return dictionary.get(key);
  }

  public LengineSet keys() {
    return LengineSet.create(dictionary.keySet());
  }

  public LengineSequence entries() {
    LinkedList<Object> sequence = new LinkedList<>();
    dictionary.entrySet()
        .stream()
        .map(entry -> LengineMapEntry.create(entry.getKey(), entry.getValue()))
        .forEach(sequence::add);

    return new LeafSequence(sequence);
  }

  public static LeafMap create() {
    return new LeafMap();
  }

  public static LeafMap create(CreateIterator seq) {
    LengineIterator it = seq.iterator();
    LeafMap map = new LeafMap();
    it.forEachRemaining(elem -> map.putEntry(LengineMapEntry.cast(elem)));
    return map;
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
  public int hashCode() {
    return dictionary.hashCode();
  }

  @Override
  public LengineIterator iterator() {
    return this.entries().iterator();
  }

  @Override
  public Long len() {
    return (long) dictionary.size();
  }

  @Override
  public LengineMapEntry head() {
    return dictionary.entrySet()
        .stream()
        .findFirst()
        .map(x -> LengineMapEntry.create(x.getKey(), x.getValue()))
        .get();
  }

  @Override
  public CreateIterator tail() {
    LengineMap.Builder builder = LengineMap.builder();

    dictionary.entrySet()
        .stream()
        .filter(entry -> !Objects.equals(entry, head()))
        .forEach(entry -> builder.put(entry.getKey(), entry.getValue()));

    return builder.build();
  }

  @Override
  public String printable() {
    return this.createStringEntry().collect(Collectors.joining(", "));
  }

  @Override
  public boolean contains(LengineMapKey set) {
    return dictionary.containsKey(set);
  }
}

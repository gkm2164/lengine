package lengine.types.collections;

import lengine.types.collections.traits.LengineIterable;
import lengine.types.collections.traits.LengineIterator;

import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * The map implementation: an immutable, insertion-ordered map. Each put copies the dictionary,
 * which is fine for the small maps scripts build and keeps every older map intact.
 */
public class LeafMap extends LengineMap {
  private final Map<LengineMapKey, Object> dictionary;

  LeafMap() {
    this(new LinkedHashMap<>());
  }

  private LeafMap(Map<LengineMapKey, Object> dictionary) {
    super(false);
    this.dictionary = dictionary;
  }

  LeafMap(LengineIterable it) {
    this();
    it.iterator().forEachRemaining(entry -> {
      LengineMapEntry e = LengineMapEntry.cast(entry);
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

  /** A new map with the entry added, or its key's value replaced. */
  public LengineMap putEntry(LengineMapEntry entry) {
    Map<LengineMapKey, Object> copy = new LinkedHashMap<>(dictionary);
    copy.put(entry.getKey(), entry.getValue());
    return new LeafMap(copy);
  }

  /** The value for key, or nil when the key is absent. */
  public Object get(LengineMapKey key) {
    Object value = dictionary.get(key);
    return value == null && !dictionary.containsKey(key) ? Nil.get() : value;
  }

  public LengineSet keys() {
    return LengineSet.create(dictionary.keySet());
  }

  public LengineSequence entries() {
    return new LeafSequence(dictionary.entrySet()
        .stream()
        .map(entry -> (Object) LengineMapEntry.create(entry.getKey(), entry.getValue()))
        .collect(Collectors.toList()));
  }

  public static LeafMap create() {
    return new LeafMap();
  }

  public static LeafMap create(LengineIterable seq) {
    return new LeafMap(seq);
  }

  public Stream<String> createStringEntry() {
    return dictionary.entrySet()
        .stream()
        .map(entry -> String.format("%s => %s", entry.getKey(), entry.getValue()));
  }

  @Override
  public String toString() {
    return this.createStringEntry().collect(Collectors.joining(", ", "{", "}"));
  }

  /** Sum of entry hashes (the java.util.Map contract), so equal maps hash alike. */
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
    Map.Entry<LengineMapKey, Object> first = dictionary.entrySet().iterator().next();
    return LengineMapEntry.create(first.getKey(), first.getValue());
  }

  @Override
  public LengineIterable tail() {
    Map<LengineMapKey, Object> rest = new LinkedHashMap<>(dictionary);
    Iterator<LengineMapKey> it = rest.keySet().iterator();
    if (it.hasNext()) {
      it.next();
      it.remove();
    }
    return new LeafMap(rest);
  }

  @Override
  public String printable() {
    return this.createStringEntry().collect(Collectors.joining(", "));
  }

  @Override
  public boolean contains(LengineMapKey key) {
    return dictionary.containsKey(key);
  }
}

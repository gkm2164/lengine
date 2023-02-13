package lengine.runtime;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Iterator;
import java.util.stream.Stream;

public class FileSequenceIterator implements LengineIterator, AutoCloseable {

  private final Stream<String> thisStream;
  private final Iterator<String> thisIterator;
  public FileSequenceIterator(String fis) {
    try {
      thisStream = Files.lines(Paths.get(fis));
      thisIterator = thisStream.iterator();
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }

  @Override
  public boolean hasNext() {
    return thisIterator.hasNext();
  }

  @Override
  public Object next() {
    return thisIterator.next();
  }

  @Override
  public void close() {
    thisStream.close();
  }
}

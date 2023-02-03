package lengine.runtime;

import lengine.util.PeekingIterator;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.stream.Stream;

public class FileSequenceIterator implements LengineIterator, AutoCloseable {

  private final Stream<String> thisStream;
  private final PeekingIterator<String> thisIterator;
  public FileSequenceIterator(String fis) {
    try {
      thisStream = Files.lines(Paths.get(fis));
      thisIterator = PeekingIterator.peekingIterator(thisStream.iterator());
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }

  @Override
  public boolean hasNext() {
    return thisIterator.hasNext();
  }

  @Override
  public Object peek() {
    return thisIterator.peek();
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

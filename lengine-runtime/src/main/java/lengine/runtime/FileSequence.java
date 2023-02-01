package lengine.runtime;

import java.io.FileNotFoundException;
import java.io.FileReader;

public class FileSequence implements CreateIterator {
  private final FileReader fis;

  public FileSequence(String fileName) {
    try {
      this.fis = new FileReader(fileName);
    } catch (FileNotFoundException e) {
      throw new RuntimeException(e);
    }
  }

  public static FileSequence create(String fileName) {
    return new FileSequence(fileName);
  }

  @Override
  public LengineIterator iterator() {
    return new FileSequenceIterator(fis);
  }

  @Override
  public Long len() {
    return null;
  }
}

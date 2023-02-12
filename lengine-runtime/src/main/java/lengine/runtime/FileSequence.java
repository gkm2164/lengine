package lengine.runtime;

public class FileSequence implements CreateIterator {
  private final String fileName;

  public FileSequence(String fileName) {
    this.fileName = fileName;
  }

  public static FileSequence create(String fileName) {
    return new FileSequence(fileName);
  }

  @Override
  public LengineIterator iterator() {
    return new FileSequenceIterator(fileName);
  }

  @Override
  public Long len() {
    return null;
  }

  @Override
  public Object head() {
    throw new RuntimeException("Unsupported operation");
  }

  @Override
  public CreateIterator tail() {
    throw new RuntimeException("Unsupported operation");
  }

  @Override
  public String toString() {
    return "[<file-sequence>]";
  }
}

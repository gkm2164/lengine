package lengine.runtime;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class FileSequenceIterator implements LengineIterator {
  private final BufferedReader br;
  private String buffer;
  public FileSequenceIterator(FileReader fis) {
    this.br = new BufferedReader(fis);
    this.buffer = "";
  }

  @Override
  public boolean hasNext() {
    try {
      buffer = this.br.readLine();
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
    return buffer != null;
  }

  @Override
  public Object next() {
    return buffer;
  }
}

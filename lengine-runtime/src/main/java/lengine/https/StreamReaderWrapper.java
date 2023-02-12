package lengine.https;

import lengine.runtime.CreateIterator;
import lengine.runtime.LengineIterator;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;

public class StreamReaderWrapper implements CreateIterator {
    private final BufferedReader stream;
    private final long bodyLength;

    public StreamReaderWrapper(InputStream stream, long bodyLength) {
        this.stream = new BufferedReader(new InputStreamReader(stream));
        this.bodyLength = bodyLength;
    }

    @Override
    public LengineIterator iterator() {
        return new StreamReaderWrapperIterator(stream);
    }

    @Override
    public Long len() {
        return bodyLength;
    }

  @Override
  public Object head() {
    throw new RuntimeException("Unsupported operation");
  }

  @Override
  public CreateIterator tail() {
    throw new RuntimeException("Unsupported operation");
  }
}

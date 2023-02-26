package lengine.https;

import lengine.collections.traits.LengineIterable;
import lengine.collections.traits.LengineIterator;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

public class StreamReaderWrapper implements LengineIterable {
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
  public LengineIterable tail() {
    throw new RuntimeException("Unsupported operation");
  }

    @Override
    public Boolean IS_NIL() {
        try {
            return !stream.ready();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}

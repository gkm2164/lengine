package lengine.https;

import lengine.runtime.LengineIterator;

import java.io.BufferedReader;
import java.io.IOException;

public class StreamReaderWrapperIterator implements LengineIterator {
    private final BufferedReader stream;

    public StreamReaderWrapperIterator(BufferedReader stream) {
        this.stream = stream;
    }

    @Override
    public boolean hasNext() {
        try {
            return stream.ready();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public Object peek() {
        throw new RuntimeException("unsupported operation");
    }

    @Override
    public Object next() {
        try {
            return (char) stream.read();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}

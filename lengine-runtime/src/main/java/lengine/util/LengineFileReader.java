package lengine.util;

import lengine.functions.LengineLambda0;
import lengine.runtime.LengineObjectType;
import lengine.runtime.LengineString;
import lengine.runtime.LengineUnit;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;

public class LengineFileReader implements LengineObjectType {
    private final InputStream inputStream;
    private boolean isClosed = false;

    public LengineFileReader(final String filename) {
        try {
            this.inputStream = new FileInputStream(filename);
        } catch (FileNotFoundException e) {
            throw new RuntimeException(e);
        }

    }

    public LengineFileReader(InputStream is) {
        this.inputStream = is;
    }

    public static LengineObjectType createFileReader(InputStream is) {
        return new LengineFileReader(is);
    }

    public static LengineObjectType createFileReader(LengineString filename) {
        return new LengineFileReader(filename.toString());
    }

    @Override
    public Object get(LengineMapKey key) {
        switch (key.getKey().toString()) {
            case "close":
                return (LengineLambda0<LengineUnit>) () -> {
                    try {
                        inputStream.close();
                        isClosed = true;
                    } catch (IOException e) {
                        throw new RuntimeException(e);
                    }
                    return LengineUnit.create();
                };
            case "get-char":
                if (isClosed) {
                    return (char) -1;
                }
                try {
                    int read = inputStream.read();
                    return (char) read;
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
            default:
                throw new RuntimeException("unknown command");
        }
    }
}

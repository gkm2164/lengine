package lengine.util;

import lengine.functions.LengineLambda0;
import lengine.runtime.LengineObjectType;
import lengine.runtime.LengineString;
import lengine.runtime.LengineUnit;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;

public class LengineFileReader implements LengineObjectType {
    private final String filename;
    private final FileInputStream inputStream;
    private boolean isClosed = false;

    public LengineFileReader(final String filename) {
        this.filename = filename;
        try {
            this.inputStream = new FileInputStream(filename);
        } catch (FileNotFoundException e) {
            throw new RuntimeException(e);
        }

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
            case "read":
                return (LengineLambda0<Character>) () -> {
                    if (isClosed) {
                        return (char) -1;
                    }
                    try {
                        int read = inputStream.read();
                        if (read == -1) {
                            inputStream.close();
                        }

                        return (char) read;
                    } catch (IOException e) {
                        throw new RuntimeException(e);
                    }
                };
            default:
                throw new RuntimeException("unknown command");
        }
    }
}

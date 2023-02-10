package lengine.https;

import lengine.functions.LengineLambda1;
import lengine.runtime.LengineObjectType;
import lengine.runtime.LengineUnit;
import lengine.util.LengineMap;

import java.io.PrintStream;

public class ResponseBuilder {
    private int statusCode;
    private LengineMap headers;

    private final PrintStream out;

    ResponseBuilder(PrintStream stream) {
        this.out = stream;
    }

    public ResponseBuilder setStatusCode(int code) {
        this.statusCode = code;
        return this;
    }

    public ResponseBuilder setHeaders(LengineMap map) {
        this.headers = map;
        return this;
    }

    public LengineObjectType build() {
        LengineLambda1<LengineUnit, Long> SET_STATUS_CODE = (code) -> {
            this.setStatusCode(code.intValue());
            return LengineUnit.create();
        };

        LengineLambda1<LengineUnit, LengineMap> SET_HEADERS = (headers) -> {
            this.setHeaders(headers);
            return LengineUnit.create();
        };

        LengineLambda1<LengineUnit, String> WRITER = (msg) -> {
            this.out.print(msg);
            return LengineUnit.create();
        };

        return (key) -> {
            switch (key.getKey()) {
                case "set-status-code":
                    return SET_STATUS_CODE;
                case "set-headers":
                    return SET_HEADERS;
                case "writer":
                    return WRITER;
                default:
                    throw new RuntimeException("Unknown accessor");
            }
        };
    }

    public int getStatusCode() {
        return this.statusCode;
    }

    public LengineMap getHeaders() {
        return this.headers;
    }
}

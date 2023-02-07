package lengine.https;

import lengine.functions.LengineLambda1;
import lengine.runtime.LengineUnit;
import lengine.util.LengineMap;
import lengine.util.LengineMapEntry;
import lengine.util.LengineMapKey;

import java.io.PrintStream;

public class ResponseBuilder {
    private LengineMap thisMap;
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

    public LengineMap build() {
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

        return LengineMap.create()
                .putEntry(LengineMapEntry.create(LengineMapKey.create("set-status-code"), SET_STATUS_CODE))
                .putEntry(LengineMapEntry.create(LengineMapKey.create("set-headers"), SET_HEADERS))
                .putEntry(LengineMapEntry.create(LengineMapKey.create("writer"), WRITER));
    }

    public int getStatusCode() {
        return this.statusCode;
    }

    public LengineMap getHeaders() {
        return this.headers;
    }
}

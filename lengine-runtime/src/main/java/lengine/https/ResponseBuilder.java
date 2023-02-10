package lengine.https;

import lengine.functions.LengineLambda1;
import lengine.runtime.LengineObjectType;
import lengine.runtime.LengineObjectWithHelp;
import lengine.runtime.LengineUnit;
import lengine.util.LengineMap;
import lengine.util.LengineMapKey;
import lengine.util.LengineSequence;

import java.io.PrintStream;
import java.util.stream.Collectors;
import java.util.stream.Stream;

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

        return new LengineObjectWithHelp() {
            @Override
            public LengineSequence help() {
                return LengineSequence.create(Stream.of("set-status-code", "set-headers", "writer")
                        .map(LengineMapKey::create)
                        .collect(Collectors.toList()));
            }

            @Override
            public String help(LengineMapKey key) {
                switch (key.getKey()) {
                    case "set-status-code":
                        return "set status code for response";
                    case "set-headers":
                        return "set header code for response";
                    case "writer":
                        return "get response body writer";
                    default:
                        throw new RuntimeException("Unknown accessor");
                }            }

            @Override
            public Object get(LengineMapKey key) {
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

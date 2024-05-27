package lengine.https;

import lengine.functions.LengineLambda1;
import lengine.types.collections.traits.LengineObjectType;
import lengine.types.collections.traits.LengineObjectWithHelp;
import lengine.types.LengineString;
import lengine.types.LengineUnit;
import lengine.types.collections.LengineMap;
import lengine.types.collections.LengineMapKey;
import lengine.types.collections.LengineSequence;

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

        LengineLambda1<LengineUnit, LengineString> WRITER = (msg) -> {
            this.out.print(msg.toString());
            return LengineUnit.create();
        };

        return new LengineObjectWithHelp() {
            @Override
            public LengineSequence help() {
                return LengineSequence.create(Stream.of("set-status-code", "set-headers", "writer")
                        .map(LengineString::create)
                        .map(LengineMapKey::create)
                        .collect(Collectors.toList()));
            }

            @Override
            public LengineString help(LengineMapKey key) {
                switch (key.getKey().toString()) {
                    case "set-status-code":
                        return LengineString.create("set status code for response");
                    case "set-headers":
                        return LengineString.create("set header code for response");
                    case "writer":
                        return LengineString.create("get response body writer");
                    default:
                        throw new RuntimeException("Unknown accessor");
                }
            }

            @Override
            public Object get(LengineMapKey key) {
                switch (key.getKey().toString()) {
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

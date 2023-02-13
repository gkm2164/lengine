package lengine.https;

import com.sun.net.httpserver.HttpExchange;
import lengine.runtime.LengineObjectType;
import lengine.runtime.LengineObjectWithHelp;
import lengine.runtime.LengineString;
import lengine.util.LeafSequence;
import lengine.util.LengineMap;
import lengine.util.LengineMapEntry;
import lengine.util.LengineMapKey;
import lengine.util.LengineSequence;
import lengine.util.Nil;

import java.io.IOException;
import java.io.InputStream;
import java.util.AbstractMap;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.util.stream.Collectors.toList;

public class RequestBuilder {
    private final String path;
    private final String query;
    private final String method;
    private final LengineMap headers;
    private final InputStream requestBody;
    private final int bodyLength;

    public RequestBuilder(HttpExchange t) {
        this.path = t.getRequestURI().getPath();
        this.query = t.getRequestURI().getQuery();
        this.method = t.getRequestMethod();
        this.requestBody = t.getRequestBody();

        LengineMap map = LengineMap.create();
        for (Map.Entry<String, List<String>> listEntry : t.getRequestHeaders().entrySet()) {
            String key = listEntry.getKey();
            List<Object> values = listEntry.getValue()
                    .stream()
                    .map(x -> (Object) x).collect(toList());

            LengineSequence seq = LeafSequence.create(values);
            map = map.putEntry(entry(key, seq));
        }

        this.headers = map;

        try {
            this.bodyLength = t.getRequestBody().available();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public LengineObjectType build() {
        return new LengineObjectWithHelp() {
            @Override
            public LengineSequence help() {
                return LengineSequence.create(Stream.of("path", "query", "method", "headers", "request-body")
                        .map(LengineString::create)
                        .map(LengineMapKey::create)
                        .collect(toList()));
            }

            @Override
            public LengineString help(LengineMapKey key) {
                switch (key.getKey().toString()) {
                    case "path":
                        return LengineString.create("get requested path");
                    case "query":
                        return LengineString.create("get requested query with parsed");
                    case "headers":
                        return LengineString.create("get headers from request");
                    case "request-body":
                        return LengineString.create("get request body in stream - note that only can be read once.");
                    default:
                        return LengineString.create("unknown operation: " + key.getKey());
                }
            }

            @Override
            public Object get(LengineMapKey key) {
                switch (key.getKey().toString()) {
                    case "path":
                        return path;
                    case "query":
                        return parseQuery(query);
                    case "method":
                        return method;
                    case "headers":
                        return headers;
                    case "request-body":
                        return new StreamReaderWrapper(requestBody, bodyLength);
                    default:
                        throw new RuntimeException("Unsupported accessor: " + key.getKey());
                }
            }
        };
    }

    private Object parseQuery(String query) {
        if (query == null || query.length() == 0) {
            return Nil.get();
        }

        Set<Map.Entry<String, List<Object>>> queryParams = Arrays.stream(query.split("&"))
                .map(str -> {
                    String[] keyValue = str.split("=");
                    String key = keyValue[0];
                    Object value = keyValue.length > 1 ? keyValue[1] : true;
                    return new AbstractMap.SimpleEntry<>(key, value);
                })
                .collect(Collectors.groupingBy(AbstractMap.SimpleEntry::getKey,
                        Collectors.mapping(AbstractMap.SimpleEntry::getValue, toList())))
                .entrySet();

        LengineMap ret = LengineMap.create();

        for (Map.Entry<String, List<Object>> queryParam : queryParams) {
            ret = ret.putEntry(entry(queryParam.getKey(), LengineSequence.create(queryParam.getValue())));
        }

        return ret;
    }

    private <T> LengineMapEntry entry(String key, T value) {
        return LengineMapEntry.create(LengineMapKey.create(LengineString.create(key)), value);
    }
}

package lengine.https;

import com.sun.net.httpserver.HttpExchange;
import lengine.util.LeafSequence;
import lengine.util.LengineMap;
import lengine.util.LengineMapEntry;
import lengine.util.LengineMapKey;
import lengine.util.LengineSequence;
import lengine.util.Nil;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.util.AbstractMap;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

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
                    .map(x -> (Object)x).collect(toList());

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

    public LengineMap build() {
        return LengineMap.create()
                .putEntry(entry("path", path))
                .putEntry(entry("query", parseQuery(query)))
                .putEntry(entry("method", method))
                .putEntry(entry("headers", headers))
                .putEntry(entry("request-body", new StreamReaderWrapper(requestBody, bodyLength)));
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
        return LengineMapEntry.create(LengineMapKey.create(key), value);
    }
}

package lengine.https;

import com.sun.net.httpserver.HttpExchange;
import lengine.util.LeafSequence;
import lengine.util.LengineMap;
import lengine.util.LengineMapEntry;
import lengine.util.LengineMapKey;
import lengine.util.LengineSequence;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

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
                    .map(x -> (Object)x).collect(Collectors.toList());

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
                .putEntry(entry("query", query))
                .putEntry(entry("method", method))
                .putEntry(entry("headers", headers))
                .putEntry(entry("request-body", new StreamReaderWrapper(requestBody, bodyLength)));
    }

    private <T> LengineMapEntry entry(String key, T value) {
        return LengineMapEntry.create(LengineMapKey.create(key), value);
    }
}

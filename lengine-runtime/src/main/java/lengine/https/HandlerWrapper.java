package lengine.https;

import com.sun.net.httpserver.Headers;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import lengine.functions.LengineLambda2;
import lengine.runtime.LengineUnit;
import lengine.util.LengineMap;
import lengine.util.LengineMapEntry;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;

public class HandlerWrapper implements HttpHandler {
    private final LengineLambda2<LengineUnit, LengineMap, LengineMap> lambda;

    public HandlerWrapper(LengineLambda2<LengineUnit, LengineMap, LengineMap> lambda) {
        this.lambda = lambda;
    }

    @Override
    public void handle(HttpExchange t) throws IOException {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        ResponseBuilder responseBuilder = new ResponseBuilder(new PrintStream(baos));

        this.lambda.invoke(LengineMap.create(), responseBuilder.build());

        t.sendResponseHeaders(responseBuilder.getStatusCode(), baos.size());
        final Headers responseHeaders = t.getResponseHeaders();
        responseBuilder.getHeaders().entries().iterator().forEachRemaining(_entry -> {
            LengineMapEntry entry = (LengineMapEntry)_entry;
            String header = entry.getKey().getKey();
            String value = (String)entry.getValue();

            responseHeaders.add(header, value);
        });
        OutputStream os = t.getResponseBody();
        os.write(baos.toByteArray());
        os.close();
    }
}

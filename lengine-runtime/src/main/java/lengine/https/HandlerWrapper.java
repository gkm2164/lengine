package lengine.https;

import com.sun.net.httpserver.Headers;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;

import lengine.types.functions.LengineLambda1;
import lengine.types.functions.LengineLambda2;
import lengine.types.collections.traits.LengineObjectType;
import lengine.types.LengineString;
import lengine.types.LengineUnit;
import lengine.types.collections.LengineMap;
import lengine.types.collections.LengineMapEntry;
import lengine.types.collections.LengineMapKey;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;

import static lengine.util.Utils.UNSAFE_cast;

public class HandlerWrapper implements HttpHandler {
  private final LengineMap lambdaMethodMapping;

  public HandlerWrapper(LengineMap lambdaMethodMapping) {
    this.lambdaMethodMapping = lambdaMethodMapping;
  }

  @Override
  public void handle(HttpExchange t) throws IOException {
    try (ByteArrayOutputStream outputStream = new ByteArrayOutputStream(); OutputStream os = t.getResponseBody()) {
      ResponseBuilder responseBuilder = new ResponseBuilder(new PrintStream(outputStream));
      RequestBuilder requestBuilder = new RequestBuilder(t);
      String method = t.getRequestMethod();

      final LengineLambda2<LengineUnit, LengineObjectType, LengineObjectType> handler;

      if (this.lambdaMethodMapping.contains(LengineMapKey.create(LengineString.create("ALL")))) {
        handler = UNSAFE_cast(this.lambdaMethodMapping.get(LengineMapKey.create(LengineString.create("ALL"))));
      } else if (this.lambdaMethodMapping.contains(LengineMapKey.create(LengineString.create(method)))) {
        handler = UNSAFE_cast(this.lambdaMethodMapping.get(LengineMapKey.create(LengineString.create(method))));
      } else {
        handler = this::nullHandler;
      }

      handler.invoke(requestBuilder.build(), responseBuilder.build());

      t.sendResponseHeaders(responseBuilder.getStatusCode(), outputStream.size());

      final Headers responseHeaders = t.getResponseHeaders();
      responseBuilder.getHeaders().entries().iterator().forEachRemaining(_entry -> {
        LengineMapEntry entry = (LengineMapEntry) _entry;
        String header = entry.getKey().getKey().toString();
        String value = entry.getValue().toString();

        responseHeaders.add(header, value);
      });
      os.write(outputStream.toByteArray());
    } catch (Exception e) {
      e.printStackTrace();
      String message = "Internal server error";
      t.sendResponseHeaders(500, message.length());
      OutputStream os = t.getResponseBody();
      os.write(message.getBytes());
      os.close();
    }
  }

  private LengineUnit nullHandler(LengineObjectType req, LengineObjectType res) {
    LengineLambda1<LengineUnit, Long> setStatusCode = UNSAFE_cast(LengineMapKey.create(LengineString.create("set-status-code")).invoke(res));
    setStatusCode.invoke(404L);
    LengineLambda1<LengineUnit, LengineMap> setHeaders = UNSAFE_cast(LengineMapKey.create(LengineString.create("set-headers")).invoke(res));
    setHeaders.invoke(LengineMap.create()
        .putEntry(LengineMapEntry.create(LengineMapKey.create(LengineString.create("Content-Type")), LengineString.create("text/html"))));
    LengineLambda1<LengineUnit, LengineString> writer = UNSAFE_cast(LengineMapKey.create(LengineString.create("writer")).invoke(res));
    writer.invoke(LengineString.create("<h1>No handler setup</h1>"));
    return LengineUnit.create();
  }
}

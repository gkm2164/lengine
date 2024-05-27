package lengine.https;

import static java.lang.String.format;
import static lengine.util.Utils.UNSAFE_cast;

import com.sun.net.httpserver.HttpServer;

import lengine.functions.LengineLambda0;
import lengine.types.LengineString;
import lengine.types.LengineUnit;
import lengine.types.collections.LengineMap;
import lengine.types.collections.LengineMapEntry;
import lengine.types.collections.LengineMapKey;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.logging.Logger;

public class HttpServerBuilder {
  private static final Logger LOG = Logger.getLogger(HttpServerBuilder.class.getName());

  public static LengineLambda0<LengineUnit> listen(LengineMap obj) {
    String address = obj.get(LengineMapKey.create(LengineString.create("host"))).toString();
    Long port = (Long) obj.get(LengineMapKey.create(LengineString.create("port")));
    LengineMap handlers = (LengineMap) obj.get(LengineMapKey.create(LengineString.create("handlers")));

    try {
      HttpServer server = HttpServer.create(new InetSocketAddress(address, port.intValue()), 0);
      handlers.entries().iterator().forEachRemaining(_entry -> {
        final LengineMapEntry entry = (LengineMapEntry) _entry;
        final LengineMap handler = UNSAFE_cast(entry.getValue());
        LOG.info(format("created context on %s", entry.getKey().getKey()));
        server.createContext(entry.getKey().getKey().toString(), new HandlerWrapper(handler));
      });
      server.setExecutor(null);
      server.start();

      LOG.info(format("Server has been started on %s:%d", address, port));
      return () -> {
        server.stop(0);
        return LengineUnit.create();
      };
    } catch (IOException e) {
      e.printStackTrace();
      throw new RuntimeException(e);
    }
  }
}

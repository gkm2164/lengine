package lengine.https;

import com.sun.net.httpserver.HttpServer;
import lengine.functions.LengineLambda2;
import lengine.runtime.LengineUnit;
import lengine.util.LengineMap;
import lengine.util.LengineMapEntry;
import lengine.util.LengineMapKey;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.logging.Logger;

public class HttpServerBuilder {
    public static void listen(LengineMap obj) {
        String address = (String) obj.get(LengineMapKey.create("host"));
        Long port = (Long) obj.get(LengineMapKey.create("port"));
        LengineMap handlers = (LengineMap) obj.get(LengineMapKey.create("handlers"));

        try {
            HttpServer server = HttpServer.create(new InetSocketAddress(address, port.intValue()), 0);
            handlers.entries().iterator().forEachRemaining(_entry -> {
                final LengineMapEntry entry = (LengineMapEntry) _entry;
                final LengineLambda2<LengineUnit, LengineMap, LengineMap> handler =
                        (LengineLambda2<LengineUnit, LengineMap, LengineMap>) entry.getValue();
                Logger.getLogger(HttpServerBuilder.class.getName())
                        .info(String.format("created context on %s", entry.getKey().getKey()));
                server.createContext(entry.getKey().getKey(), new HandlerWrapper(handler));
            });
            server.setExecutor(null);
            server.start();

            while(true);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

    }
}

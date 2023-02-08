package lengine.https;

import com.sun.net.httpserver.HttpServer;
import lengine.functions.LengineLambda0;
import lengine.runtime.LengineUnit;
import lengine.util.LengineMap;
import lengine.util.LengineMapEntry;
import lengine.util.LengineMapKey;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.logging.Logger;

public class HttpServerBuilder {
    public static LengineLambda0<LengineUnit> listen(LengineMap obj) {
        String address = (String) obj.get(LengineMapKey.create("host"));
        Long port = (Long) obj.get(LengineMapKey.create("port"));
        LengineMap handlers = (LengineMap) obj.get(LengineMapKey.create("handlers"));

        try {
            HttpServer server = HttpServer.create(new InetSocketAddress(address, port.intValue()), 0);
            handlers.entries().iterator().forEachRemaining(_entry -> {
                final LengineMapEntry entry = (LengineMapEntry) _entry;
                final LengineMap handler = UNSAFE_cast(entry.getValue());
                Logger.getLogger(HttpServerBuilder.class.getName())
                        .info(String.format("created context on %s", entry.getKey().getKey()));
                server.createContext(entry.getKey().getKey(), new HandlerWrapper(handler));
            });
            server.setExecutor(null);
            server.start();

            return () -> {
                server.stop(0);
                return LengineUnit.create();
            };
        } catch (IOException e) {
            e.printStackTrace();
            throw new RuntimeException(e);
        }
    }

    static <T> T UNSAFE_cast(Object object) {
        return (T)object;
    }
}

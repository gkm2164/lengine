package lengine.util;

import lengine.functions.LengineLambda0;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

public class LengineFuture {
    private static final ExecutorService es = Executors.newCachedThreadPool();

    private final Future<Object> future;

    public LengineFuture(LengineLambda0<?> thisRun) {
        this.future = es.submit(thisRun::invoke);
    }

    public Object await() {
        try {
            return this.future.get();
        } catch (ExecutionException | InterruptedException e) {
            throw new RuntimeException(e);
        }
    }
}

package lengine.concurrency;

import lengine.functions.LengineLambda0;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class LengineFuture {
    private static final ExecutorService es = Executors.newCachedThreadPool();

    private final CompletableFuture<Object> future;

    public LengineFuture(LengineLambda0<?> thisRun) {
        this.future = CompletableFuture.supplyAsync(thisRun::invoke, es);
    }

    public Object await() {
        return this.future.join();
    }
}

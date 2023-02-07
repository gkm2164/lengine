package lengine.concurrency;

import java.util.concurrent.SynchronousQueue;

public class LengineChannel {
    private final SynchronousQueue<Object> messageQueue;

    private LengineChannel() {
        this.messageQueue = new SynchronousQueue<>(true);
    }

    public static LengineChannel create() {
        return new LengineChannel();
    }

    public void sendMessage(Object obj) throws InterruptedException {
        this.messageQueue.put(obj);
    }

    public Object receiveMessage() throws InterruptedException {
        return this.messageQueue.take();
    }
}

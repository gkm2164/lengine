package lengine.concurrency;

import java.util.concurrent.SynchronousQueue;
import java.util.concurrent.atomic.AtomicBoolean;

import lengine.util.Nil;

public class LengineChannel {
  static class ChannelClosed { }

  AtomicBoolean closing = new AtomicBoolean(false);
  AtomicBoolean isClosed = new AtomicBoolean(false);
  private final SynchronousQueue<Object> messageQueue;

  private LengineChannel() {
    this.messageQueue = new SynchronousQueue<>(true);
  }

  public static LengineChannel create() {
    return new LengineChannel();
  }

  public void sendMessage(Object obj) throws InterruptedException {
    if (closing.get()) {
      System.out.println("Can't send data to closed channel anymore");
      return;
    }
    this.messageQueue.put(obj);
  }

  public Object receiveMessage() throws InterruptedException {
    if (isClosed.get()) {
      return Nil.get();
    }

    Object ret = this.messageQueue.take();
    if (ret instanceof ChannelClosed) {
      isClosed.set(true);
      return Nil.get();
    }

    return ret;
  }

  public void close() throws InterruptedException {
    sendMessage(new ChannelClosed());
    closing.set(true);
  }
}

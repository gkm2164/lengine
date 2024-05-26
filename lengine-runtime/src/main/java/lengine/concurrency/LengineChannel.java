package lengine.concurrency;

import lengine.collections.Nil;

import java.util.Queue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.atomic.AtomicReference;


public class LengineChannel {
  static class ChannelClosed { }

  enum ChannelState {
    RUNNING,
    CLOSING,
    CLOSED
  }

  final AtomicReference<ChannelState> channelState = new AtomicReference<>(ChannelState.RUNNING);
  private final Queue<Object> messageQueue;

  private LengineChannel() {
    this.messageQueue = new LinkedBlockingQueue<>();
  }

  public static LengineChannel create() {
    return new LengineChannel();
  }

  public void sendMessage(Object obj) throws InterruptedException {
    synchronized (channelState) {
      if (channelState.get() != ChannelState.RUNNING) {
        throw new RuntimeException("Send failure: this channel is already closed");
      }
    }

    this.messageQueue.add(obj);
  }

  public Object receiveMessage() throws InterruptedException {
    synchronized (channelState) {
      if (channelState.get() == ChannelState.CLOSED) {
        throw new RuntimeException("Receive failure: this channel is already closed");
      }
    }

    Object object = this.messageQueue.poll();
    if (object instanceof ChannelClosed) {
      synchronized (channelState) {
        channelState.set(ChannelState.CLOSED);
        return Nil.get();
      }
    }

    return object;
  }

  public void close() throws InterruptedException {
    synchronized (channelState) {
      channelState.set(ChannelState.CLOSING);
    }
    this.messageQueue.add(new ChannelClosed());
  }
}

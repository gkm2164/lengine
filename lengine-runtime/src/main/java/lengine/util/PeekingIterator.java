package lengine.util;

import java.util.Iterator;

public class PeekingIterator<E> implements Iterator<E> {

    /** The iterator being decorated. */
    private final Iterator<? extends E> iterator;

    /** Indicates that the decorated iterator is exhausted. */
    private boolean exhausted = false;

    /** Indicates if the lookahead slot is filled. */
    private boolean slotFilled = false;

    /** The current slot for lookahead. */
    private E slot;

    //-----------------------------------------------------------------------
    /**
     * Decorates the specified iterator to support one-element lookahead.
     * <p>
     * If the iterator is already a {@link PeekingIterator} it is returned directly.
     *
     * @param <E>  the element type
     * @param iterator  the iterator to decorate
     * @return a new peeking iterator
     * @throws NullPointerException if the iterator is null
     */
    public static <E> PeekingIterator<E> peekingIterator(final Iterator<? extends E> iterator) {
        if (iterator == null) {
            throw new NullPointerException("Iterator must not be null");
        }
        if (iterator instanceof PeekingIterator<?>) {
            @SuppressWarnings("unchecked") // safe cast
            final PeekingIterator<E> it = (PeekingIterator<E>) iterator;
            return it;
        }
        return new PeekingIterator<>(iterator);
    }

    //-----------------------------------------------------------------------

    /**
     * Constructor.
     *
     * @param iterator  the iterator to decorate
     */
    public PeekingIterator(final Iterator<? extends E> iterator) {
        this.iterator = iterator;
    }

    private void fill() {
        if (exhausted || slotFilled) {
            return;
        }
        if (iterator.hasNext()) {
            slot = iterator.next();
            slotFilled = true;
        } else {
            exhausted = true;
            slot = null;
            slotFilled = false;
        }
    }

    //-----------------------------------------------------------------------
    @Override
    public boolean hasNext() {
        if (exhausted) {
            return false;
        }
        return slotFilled || iterator.hasNext();
    }

    /**
     * Returns the next element in iteration without advancing the underlying iterator.
     * If the iterator is already exhausted, null will be returned.
     * <p>
     * Note: this method does not throw a {@link NoSuchElementException} if the iterator
     * is already exhausted. If you want such a behavior, use {@link #element()} instead.
     * <p>
     * The rationale behind this is to follow the {@link java.util.Queue} interface
     * which uses the same terminology.
     *
     * @return the next element from the iterator
     */
    public E peek() {
        fill();
        return exhausted ? null : slot;
    }

    /**
     * Returns the next element in iteration without advancing the underlying iterator.
     * If the iterator is already exhausted, null will be returned.
     *
     * @return the next element from the iterator
     * @throws NoSuchElementException if the iterator is already exhausted according to {@link #hasNext()}
     */
    public E element() {
        fill();
        if (exhausted) {
            throw new NoSuchElementException();
        }
        return slot;
    }

    @Override
    public E next() {
        if (!hasNext()) {
            throw new NoSuchElementException();
        }
        final E x = slotFilled ? slot : iterator.next();
        // reset the lookahead slot
        slot = null;
        slotFilled = false;
        return x;
    }

    /**
     * {@inheritDoc}
     *
     * @throws IllegalStateException if {@link #peek()} or {@link #element()} has been called
     *   prior to the call to {@link #remove()}
     */
    @Override
    public void remove() {
        if (slotFilled) {
            throw new IllegalStateException("peek() or element() called before remove()");
        }
        iterator.remove();
    }

    static class NoSuchElementException extends RuntimeException {
    }
}

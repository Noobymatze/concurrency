package com.github.noobymatze.concurrency;

import java.util.Objects;
import java.util.function.Supplier;

/**
 * A Channel encapsulates the communication between two threads. It builds
 * upon the logic implemented in a {@link MVar}.
 *
 * @param <T> The datatype, which can be sent over this channel.
 */
public final class Channel<T> {

    /**
     * References the item, that sits at the start of this
     * Channel.
     */
    private final MVar<MVar<Item<T>>> start;

    /**
     * References the item, that sits at the end of this channel,
     * for easier write access.
     */
    private final MVar<MVar<Item<T>>> end;

    /**
     * Creates a new Channel, referencing an empty MVar.
     */
    public Channel() {
        MVar<Item<T>> last = MVar.empty();
        this.start = MVar.withDefault(last);
        this.end = MVar.withDefault(last);
    }

    /**
     * Consumes a value from this channel, thus deleting it
     * from it. Blocks, if there is none.
     * 
     * @return The Element, that is currently at the beginning of
     * this Channel.
     * @throws InterruptedException If any thread interrupted the current one
     * during synchronization.
     */
    public T read() throws InterruptedException {
        MVar<Item<T>> firstItem = start.take();
        Item<T> item = firstItem.take();
        start.put(item.next());
        return item.get();
    }

    /**
     * Writes a new value to the end of the channel.
     * 
     * @param o The object to be put into the current channel.
     * @throws InterruptedException If any thread interrupted the current one
     * during synchronization.
     */
    public void write(T o) throws InterruptedException {
        MVar<Item<T>> newItem = MVar.empty();
        MVar<Item<T>> lastItem = end.take();
        lastItem.put(new Item<>(o, newItem));
        end.put(newItem);
    }

    /**
     * Tests, whether the current instance is empty.
     * 
     * @return True, if it is empty, false otherwise.
     * @throws java.lang.InterruptedException
     */
    public boolean isEmpty() throws InterruptedException {
        return start.read().equals(end.read());
    }

    /**
     * Adds a new item add the beginning of this Channel.
     * 
     * ist kaputt, kann behoben werden durch einen weiteren
     * unget Pointer.
     * @param o The value of the newly created item.
     * @throws InterruptedException 
     */
    public void unGet(T o) throws InterruptedException {
        MVar<Item<T>> newItem = MVar.empty();
        MVar<Item<T>> firstItem = start.take();
        newItem.put(new Item<>(o, firstItem));
        start.put(newItem);
    }

    /**
     * An item, that is used for storing a value in this Channel.
     * 
     * @param <T> The datatype of the items value.
     */
    private static final class Item<T> implements Supplier<T> {

        private final T value;
        private final MVar<Item<T>> next;

        /**
         * Creates a new Item, that will wait in a Channel for consumption.
         * 
         * @param value The value this Item holds, MUST be set.
         * @param next The following Item, MUST be set.
         */
        public Item(T value, MVar<Item<T>> next) {
            this.value = Objects.requireNonNull(value);
            this.next = Objects.requireNonNull(next);
        }

        /**
         * Return the following Item.
         * 
         * @return Next Item.
         */
        public MVar<Item<T>> next() {
            return next;
        }

        @Override
        public T get() {
            return value;
        }

    }
    
}

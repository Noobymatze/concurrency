package com.github.noobymatze.concurrency;

import static java.util.Objects.isNull;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * The MVar serves as a building block for communication between
 * threads.
 * 
 * There are two states the MVar can be in:
 * Either it contains a value, which can be taken from another
 * Thread or it is empty and therefore needs to be filled with a value.
 * 
 * @param <T>
 */
public final class MVar<T> {

    private static final Logger LOGGER = Logger.getLogger(MVar.class.getName());

    /**
     * The value which can be set by different threads.
     */
    private T value;

    /**
     * The readerLock must be used, whenever multiple threads are
     * able to read the value of MVar and the access needs to be synchronized.
     */
    private final Object readerLock = new Object();

    /**
     * The writerLock must be used, whenever multiple threads are
     * able to write the MVar value and the access needs to be synchronized.
     */
    private final Object writerLock = new Object();

    private MVar() {}

    /**
     * Puts a new value into the current instance in a thread-safe manner.
     * 
     * Notifies waiting reader-Threads of the newly set value.
     * 
     * @param value The value to become the content of this container.
     */
    public void put(T value) {
        synchronized (writerLock) {
            while(!isEmpty()) {
                try {
                    writerLock.wait();
                } catch (InterruptedException ex) {
                    LOGGER.log(Level.SEVERE, null, ex);
                }
            }

            synchronized (readerLock) {
                this.value = value;
                readerLock.notify(); // Notify reading threads of new value.
            }
        }
    }

    /**
     * Returns the current value in a thread-safe manner and sets the value
     * to NULL. If there is no value, this method waits for one to be set.
     * 
     * @return The current value.
     */
    public T take() {
        synchronized (readerLock) {
            while (isEmpty()) {
                try {
                    readerLock.wait();
                } catch (InterruptedException ex) {
                    LOGGER.log(Level.SEVERE, null, ex);
                }
            }

            synchronized (writerLock) {
                T result = value;
                value = null;
                writerLock.notify(); // Notify writing threads of empty value. 
                return result;
            }
        }
    }

    /**
     * Checks whether the current instance contains a non-empty value.
     * 
     * @return True, if the current value is NULL, false otherwise.
     */
    public boolean isEmpty() {
        return isNull(value);
    }
    
    /**
     * Initializes an empty MVar.
     * 
     * @param <T> The values type.
     * @return A newly created instance of an empty MVar.
     */
    public static <T> MVar<T> empty() {
        return new MVar<>();
    }

    /**
     * Initializes a MVar with the given default value,
     * that can be taken from a consumer.
     * 
     * @param <T> The values type.
     * @param value Default value
     * @return A newly created instance of a filled MVar.
     */
    public static <T> MVar<T> withDefault(T value) {
        MVar<T> mVar = new MVar<>();
        mVar.value = value;
        return mVar;
    }
    
}

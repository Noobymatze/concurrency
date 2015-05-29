package com.github.noobymatze.concurrency;

/**
 *
 */
public final class SyncVar<T> {

    private boolean writer;
    private boolean reader;

    private T value;

    private final Object readerLock = new Object();
    private final Object writerLock = new Object();

    //  T2
    public void put(T value) throws InterruptedException {
        synchronized (writerLock) {
            writer = true;
            synchronized (this) {
                this.notify();
                while(!reader) {
                    this.wait(); // T1
                }

                this.value = value;
                readerLock.notify();
                writer = false;
            }
        }
    }

    //  
    public T take() throws InterruptedException {
        synchronized (readerLock) {
            reader = true;
            synchronized (this) {
                this.notify(); 
                while (!writer) {
                    this.wait(); 
                }

                T result = value;
                value = null;
                reader = false;
                return result;
            }
        }
    }//T3,T4

}

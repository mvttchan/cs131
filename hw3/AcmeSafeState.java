import java.util.concurrent.atomic.AtomicLongArray;

class AcmeSafeState implements State {
    private long[] value;
    private AtomicLongArray array;

    AcmeSafeState(int length) {
    	array = new AtomicLongArray(length);
	value = new long[length];
    }

    public int size() { return value.length; }

    public long[] current() { return value; }

    public synchronized void swap(int i, int j) {
	value[i] = array.decrementAndGet(i);
	value[j] = array.incrementAndGet(j);
    }
}

package queue;

public class ArrayQueue extends AbstractQueue {
    private int head = 0;
    private int tail = 0;
    private Object[] elements = new Object[4];

    protected void pushImpl(Object e) {
        ensureCapacity(size() + 1);
        if (tail == 0) {
            elements[elements.length - 1] = e;
            tail = elements.length - 1;
        } else {
            elements[tail - 1] = e;
            tail--;
        }
    }

    protected Object peekImpl() {
        return elements[head == 0 ? elements.length - 1 : head - 1];
    }

    protected Object removeImpl() {
        Object r = elements[head == 0 ? elements.length - 1 : head - 1];
        elements[head == 0 ? elements.length - 1 : head - 1] = null;
        if (head == 0) {
            head = elements.length - 1;
        } else {
            head--;
        }
        return r;
    }

    protected void enqueueImpl(Object e) {
        ensureCapacity(size() + 1);
        elements[head++] = e;
        head %= elements.length;
    }

    protected Object dequeImpl() {
        Object res = elements[tail];
        elements[tail] = null;
        tail++;
        tail %= elements.length;
        ensureCapacity(size());
        return res;
    }

    private void ensureCapacity(int capacity) {
        if (capacity == elements.length) {
            Object[] temp = new Object[elements.length * 2];
            if (tail <= head) {
                System.arraycopy(elements, tail, temp, 0, size());
            } else {
                System.arraycopy(elements, tail, temp, 0, elements.length - tail);
                System.arraycopy(elements, 0, temp, elements.length - tail, head);
            }
            head = size();
            tail = 0;
            elements = temp;
        }
    }

    protected Object elementImpl() {
        return elements[tail];
    }

    protected void clearImpl() {
        elements = new Object[4];
        head = 0;
        tail = 0;
    }


}

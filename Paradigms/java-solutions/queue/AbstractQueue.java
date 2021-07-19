package queue;

//import javax.management.ObjectName;

public abstract class AbstractQueue implements Queue {
    protected int size = 0;

    public boolean contains(Object e) {
        return leaveOrNot(e, false);
    }

    private boolean leaveOrNot(Object e, boolean check) {
        assert e != null;

        int cnt = 0;
        int size_t = size;

        while (size_t != 0){
            Object temp = dequeue();
            size_t--;
            if (temp.equals(e)){
                cnt++;
                if (!check || cnt != 1) {
                    enqueue(temp);
                }
            } else {
                enqueue(temp);
            }
        }
        check = false;
        return cnt != 0;
    }

    // :NOTE:вынести общий код - DONE
    public boolean removeFirstOccurrence(Object e){
        return leaveOrNot(e, true);
    }

    @Override
    public int size() {
        return size;
    }

    @Override
    public boolean isEmpty() {
        return size == 0;
    }

    @Override
    public void enqueue(final Object e) {
        assert e != null : "Null element";

        enqueueImpl(e);

        size++;
    }
    protected abstract void enqueueImpl(final Object e);

    @Override
    public Object dequeue() {
        assert size > 0 : "Empty queue";

        Object res = dequeImpl();
        size--;

        return res;
    }

    protected abstract Object dequeImpl();

    @Override
    public Object element() {
        assert size != 0 : "No elements";
        return elementImpl();
    }

    protected abstract Object elementImpl();

    @Override
    public void clear() {
        clearImpl();

        size = 0;
    }

    protected abstract void clearImpl();

    @Override
    public Object remove() {
        assert !isEmpty();

        size--;
        return removeImpl();
    }

    protected abstract Object removeImpl();

    @Override
    public Object peek() {
        assert size != 0;
        return peekImpl();
    }

    protected abstract Object peekImpl();

    @Override
    public void push(Object e) {
        assert e != null : "Null element";

        pushImpl(e);

        size++;
    }

    protected abstract void pushImpl(final Object e);

}

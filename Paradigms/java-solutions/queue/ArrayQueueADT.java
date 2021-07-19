package queue;

import java.util.Arrays;

/*
Model:
    [a1,...,an]
    n - размер очереди

Inv:
    n >= 0
    forall i = 1..n: a[i] != null

Let: Immutable: n == n' && forall i = 1..n: a'[i] == a[i]

Pred: e != null
Post: n = n' + 1 && a[n] == e &&
      forall i = 1..n': a'[i] == a[i]
enqueue(e)

Pred: n >= 1
Post: R == a[1] && Immutable
element()

Pred: n >= 1
Post: R == a[1] && n == n' - 1 &&
      forall i = 1..n: a[i] = a'[i + 1]
dequeue()

Pred: true
Post: R == n && Immutable
size()

Pred: true
Post: R == (n == 0) && Immutable
isEmpty()

Pred: true
Post: n == 0
clear()

 */
public class ArrayQueueADT {
    private int tail = 0;
    private int head = 0;
    private Object[] elements = new Object[16];

	/* Pred: true
	   Post: queue - new
	*/
    public static ArrayQueueADT create() {
        return new ArrayQueueADT();
    }
	
	/*
    Pred: e != null && queue != null
    Post: n = n' + 1 && a[1] == e &&
          forall i = 2..n' : a[i] == a'[i - 1]
     */
    public static void push(ArrayQueueADT queue, Object e) {
        assert e != null : "Null element";
        ensureCapacity(queue, size(queue) + 1);
        if (queue.head == 0) {
            queue.elements[queue.elements.length - 1] = e;
            queue.head = queue.elements.length - 1;
        } else {
            queue.elements[queue.head - 1] = e;
            queue.head--;
        }
    }

    /*Pred: n >= 1 && queue != null
      Post: R == a[n] && Immutable
     */
    public static Object peek(ArrayQueueADT queue) {
        assert !isEmpty(queue);
        return queue.elements[queue.tail == 0 ? queue.elements.length - 1 : queue.tail - 1];
    }

    /*Pred: n >= 1 && queue != null
      Post: R == a[n] && n = n' - 1 &&
            forall i = 1..n : a[i] == a'[i] && a[n'] == null
     */
    public static Object remove(ArrayQueueADT queue) {
        assert !isEmpty(queue);
        Object r = queue.elements[queue.tail == 0 ? queue.elements.length - 1 : queue.tail - 1];
        queue.elements[queue.tail == 0 ? queue.elements.length - 1 : queue.tail - 1] = null;
        if (queue.tail == 0) {
            queue.tail = queue.elements.length - 1;
        } else {
            queue.tail--;
        }
        return r;
    }

    /*
    Pred: e != null && queue != null
    Post: n = n' + 1 && a[n] == e &&
          forall i = 1..n': a'[i] == a[i]
     */
    public static void enqueue(ArrayQueueADT queue, Object e) {
        assert e != null : "Null element";
        ensureCapacity(queue, size(queue) + 1);
        queue.elements[queue.tail++] = e;
        queue.tail %= queue.elements.length;
    }

    /*
    Pred: n >= 1 && queue != null
    Post: R == a[1] && n == n' - 1 &&
          forall i = 1..n: a[i] = a'[i + 1]
    */
    public static Object dequeue(ArrayQueueADT queue) {
        assert size(queue) > 0 : "Empty queue";
        Object res = queue.elements[queue.head];
        queue.elements[queue.head] = null;
        queue.head++;
        queue.head %= queue.elements.length;
        return res;
    }
	
	/*
	Pred: queue != null
	*/
    private static void ensureCapacity(ArrayQueueADT queue, int capacity) {
        if (capacity == queue.elements.length) {
            Object[] temp = new Object[queue.elements.length * 2];
            if (queue.head <= queue.tail) {
                System.arraycopy(queue.elements, queue.head, temp, 0, size(queue));
            } else {
                System.arraycopy(queue.elements, queue.head, temp, 0, queue.elements.length - queue.head);
                System.arraycopy(queue.elements, 0, temp, queue.elements.length - queue.head, queue.tail);
            }
            queue.tail = size(queue);
            queue.head = 0;
            queue.elements = temp;
        }
    }

    /* 
    Pred: n >= 1 && queue != null
    Post: R == a[1] && Immutable
     */
    public static Object element(ArrayQueueADT queue) {
        assert size(queue) != 0 : "No elements";
        return queue.elements[queue.head];
    }

    /*
    Pred: true && queue != null
    Post: R == n && Immutable
     */
    public static int size(ArrayQueueADT queue) {
        return queue.tail - queue.head + (queue.head <= queue.tail ? 0 : queue.elements.length);
    }

    /*
    Pred: true && queue != null
    Post: n == 0
     */
    public static void clear(ArrayQueueADT queue) {
        queue.elements = new Object[10];
        queue.head = 0;
        queue.tail = 0;
    }

    /*
    Pred: true && queue != null
    Post: R == (n == 0) && Immutable
     */
    public static boolean isEmpty(ArrayQueueADT queue) {
        return size(queue) == 0;
    }
}
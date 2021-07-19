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
public class ArrayQueueModule {
    private static int tail = 0;
    private static int head = 0;
    private static Object[] elements = new Object[10];

    /*
    Pred: e != null
    Post: n = n' + 1 && a[1] == e &&
          forall i = 2..n' : a[i] == a'[i - 1]
     */
    public static void push(Object e) {
        assert e != null : "Null element";
        ensureCapacity(size() + 1);
        if (head == 0){
            elements[elements.length - 1] = e;
            head = elements.length -  1;
        } else {
            elements[head - 1] = e;
            head--;
        }
    }

    /*Pred: n >= 1
      Post: R == a[n] && Immutable
     */
    public static Object peek(){
        assert !isEmpty();
        return elements[tail == 0 ? elements.length - 1 : tail - 1];
    }

    /*Pred: n >= 1
      Post: R == a[n] && n = n' - 1 &&
            forall i = 1..n : a[i] == a'[i] && a[n'] == null
     */
    public static Object remove(){
        assert !isEmpty();
        Object r = elements[tail == 0 ? elements.length - 1 : tail - 1];
        elements[tail == 0 ? elements.length - 1 : tail - 1] = null;
        if (tail == 0){
            tail = elements.length - 1;
        } else {
            tail--;
        }
        return r;
    }

    /*
    Pred: e != null
    Post: n = n' + 1 && a[n] == e &&
          forall i = 1..n': a'[i] == a[i]
     */
    public static void enqueue(Object e) {
        assert e != null : "Null element";
        ensureCapacity(size() + 1);
        elements[tail++] = e;
        tail %= elements.length;
    }

    /*
    Pred: n >= 1
    Post: R == a[1] && n == n' - 1 &&
          forall i = 1..n: a[i] = a'[i + 1]
     */
    public static Object dequeue() {
        assert size() > 0 : "Empty queue";
        Object res = elements[head];
        elements[head] = null;
        head++;
        head %= elements.length;
        return res;
    }

    private static void ensureCapacity(int capacity) {
        if (capacity == elements.length) {
            Object[] temp = new Object[elements.length * 2];
            if (head <= tail) {
                System.arraycopy(elements, head, temp, 0, size());
            } else {
                System.arraycopy(elements, head, temp, 0, elements.length - head);
                System.arraycopy(elements, 0, temp, elements.length - head, tail);
            }
            tail = size();
            head = 0;
            elements = temp;
        }
    }

    /*
    Pred: n >= 1
    Post: R == a[1] && Immutable
     */
    public static Object element() {
        assert size() != 0 : "No elements";
        return elements[head];
    }

    /*
    Pred: true
    Post: R == n && Immutable
     */
    public static int size() {
        return tail - head + (head <= tail ? 0 : elements.length);
    }

    /*
    Pred: true
    Post: n == 0
     */
    public static void clear() {
        elements = new Object[10];
        head = 0;
        tail = 0;
    }

    /*
    Pred: true
    Post: R == (n == 0) && Immutable
     */
    public static boolean isEmpty() {
        return size() == 0;
    }
}

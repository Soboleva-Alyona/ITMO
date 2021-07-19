package queue;

/*
Model:
    [a1,...,an]
    n - размер очереди

Inv:
    n >= 0
    forall i = 1..n: a[i] != null

Let: Immutable: n == n' && forall i = 1..n: a'[i] == a[i]
*/

public interface Queue {
    /*
    Pred: true
    Post: R == n && Immutable
     */
    int size();

    /*
    Pred: true
    Post: R == (n == 0) && Immutable
     */
    boolean isEmpty();

    /*
    Pred: e != null
    Post: n = n' + 1 && a[n] == e &&
         forall i = 1..n': a'[i] == a[i]
    */
    void enqueue(Object e);

    /*
    Pred: n >= 1
    Post: R == a[1] && n == n' - 1 &&
          forall i = 1..n: a[i] = a'[i + 1]
     */
    Object dequeue();

    /*
    Pred: n >= 1
    Post: R == a[1] && Immutable
     */
    Object element();

    /*
    Pred: true
    Post: n == 0
     */
    void clear();

    /*
    Pred: n >= 1
    Post: R == a[n] && n = n' - 1 &&
          forall i = 1..n : a[i] == a'[i] && a[n'] == null
     */
    Object remove();

    /*
    Pred: n >= 1
    Post: R == a[n] && Immutable
     */
    Object peek();

    /*
    Pred: e != null
    Post: n = n' + 1 && a[1] == e &&
          forall i = 2..n' : a[i] == a'[i - 1]
     */
    void push(Object e);

    /*
    Pred: e != null
    Post: Immutable && Res = (exists i : a[i] == e)
     */
    boolean contains(Object e);

    /*
    Pred: e != null
    Post: Res = (exists i'' : a[i''] == e) && (n = n' - (Res) &&
          forall i < i'' && i < n : a[i] = a'[i] && forall i >= i'' && i < n : a[i] = a'[i + 1])
     */
    // :NOTE: упростить контракт -DONE
    boolean removeFirstOccurrence(Object e);


}

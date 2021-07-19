package queue;

public class LinkedQueue extends AbstractQueue {
    private Node head;
    private Node tail;

    private static class Node{
        private Node next;
        private final Object value;

        public Node(Node next, Object value){
            this.next = next;
            this.value = value;
        }
    }

    protected void pushImpl(Object e) {
        if (size == 0) {
            head = tail = new Node(null, e);
        } else {
            head = new Node(head, e);
        }
    }

    protected Object peekImpl() {
        return tail;
    }


    protected Object removeImpl() {
        Object r = tail.value;
        tail = null;
        return r;
    }

    protected void enqueueImpl(Object e) {
        if (size == 0) {
            head = tail = new Node(null, e);
        } else {
            tail.next = new Node(null, e);
            tail = tail.next;
        }
    }

    protected Object dequeImpl() {
        Object res = head.value;
        head = head.next;
        return res;
    }

    protected Object elementImpl() {
        return head.value;
    }

    protected void clearImpl() {
        head = null;
        tail = null;
    }

}

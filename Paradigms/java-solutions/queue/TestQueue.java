package queue;

public class TestQueue {
    public static void main(String[] args) {
        ArrayQueue queue2 = new ArrayQueue();
        AbstractQueue queue1 = new LinkedQueue();


        fill(queue1);
        fill(queue2);

        queue1.clear();
        queue2.clear();

        fill(queue1);
        System.out.println(queue1.contains(2));

        queue1.removeFirstOccurrence(3);
        dump(queue1);
        //System.out.println(queue1.size());
        fill(queue2);
        //dump(queue2);
    }

    private static void dump(final AbstractQueue queue) {
        while (!queue.isEmpty()) {
            System.out.println(queue.dequeue());
        }
    }

    private static void fill(final AbstractQueue queue) {
        for (int i = 0; i < 5; i++) {
            queue.enqueue(i * 2);
            queue.enqueue(3);
        }
    }
}

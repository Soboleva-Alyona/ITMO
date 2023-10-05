import java.util.concurrent.atomic.*

class Solution(val env: Environment) : Lock<Solution.Node> {
    val tail = AtomicReference<Node>(null)

    override fun lock(): Node {
        val my = Node() // сделали узел

        my.locked.value = true
        val pred = tail.getAndSet(my)
        if (pred != null) {
            pred.next.compareAndSet(null, my)
            while (my.isLocked()) env.park()
        }

        return my // вернули узел
    }

    override fun unlock(node: Node) {
        if (node.next.value == null) {
            if (tail.compareAndSet(node, null)) {
                return
            } else {
                while (node.next.value == null) {
                    // wait
                }
            }
        }
        val nextValue = node.next.value
        if (nextValue != null) {
            nextValue.locked.compareAndSet(true, false)
            env.unpark(nextValue.thread)
        }
    }

    class Node {
        val thread = Thread.currentThread() // запоминаем поток, которые создал узел
        val locked = AtomicReference<Boolean>(false)
        val next = AtomicReference<Node?>(null)

        fun isLocked() = this.locked.value
    }
}
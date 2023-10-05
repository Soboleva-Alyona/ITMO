package mpp.msqueue

import kotlinx.atomicfu.AtomicRef
import kotlinx.atomicfu.atomic

class MSQueue<E> {
    private val head: AtomicRef<Node<E>>
    private val tail: AtomicRef<Node<E>>

    init {
        val dummy = Node<E>(null)
        head = atomic(dummy)
        tail = atomic(dummy)
    }

    /**
     * Adds the specified element [x] to the queue.
     */
    fun enqueue(x: E) {
        val node = Node(x)
        while (true) {
            val curTail = tail.value
            if (curTail.next.compareAndSet(null, node)) {
                tail.compareAndSet(curTail, node)
                return
            } else {
                curTail.next.value?.let { tail.compareAndSet(curTail, it) }
            }
        }
    }

    /**
     * Retrieves the first element from the queue
     * and returns it; returns `null` if the queue
     * is empty.
     */
    fun dequeue(): E? {
        while (true) {
            val curHead = head.value
            val curTail = tail.value
            val next = curHead.next.value
            if (curHead == curTail) {
                if (next == null) {
                    return null
                }
                tail.compareAndSet(curTail, next)
            } else {
                val res = next?.x
                if (next?.let { head.compareAndSet(curHead, it) } == true) {
                    return res
                }
            }
        }
    }

    fun isEmpty(): Boolean {
        val curHead = head.value
        val curTail = tail.value
        val next = curHead.next.value
        return curHead == curTail && next == null
    }
}

private class Node<E>(val x: E?) {
    val next = atomic<Node<E>?>(null)
}
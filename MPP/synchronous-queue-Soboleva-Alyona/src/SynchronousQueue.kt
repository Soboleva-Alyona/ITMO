import kotlinx.atomicfu.AtomicRef
import kotlinx.atomicfu.atomic
import kotlin.coroutines.Continuation
import kotlin.coroutines.resume
import kotlin.coroutines.suspendCoroutine

/**
 * An element is transferred from sender to receiver only when [send] and [receive]
 * invocations meet in time (rendezvous), so [send] suspends until another coroutine
 * invokes [receive] and [receive] suspends until another coroutine invokes [send].
 */
class SynchronousQueue<E> {
    private val head: AtomicRef<Node<E>>
    private val tail: AtomicRef<Node<E>>

    init {
        val dummy = Node<E>(null)
        head = atomic(dummy)
        tail = atomic(dummy)
    }

    private fun dequeueForOperation(h: Node<E>): Node<E> {
        val next = h.next.value ?: return Node(null, OperationType.DUMP)
        if (this.head.compareAndSet(h, next)) {
            return next
        } else {
            return Node(null, OperationType.DUMP)
        }
    }

    /**
     * Sends the specified [element] to this channel, suspending if there is no waiting
     * [receive] invocation on this channel.
     */
    suspend fun send(element: E) {
        while (true) {
            val h = head.value
            val t = tail.value
            if (isEmpty(h, t) || t.isSender()) {
                // enqueueAndSuspend
                if (suspendCoroutine cont@{ continuation ->
                        val offer = Node(element, OperationType.SEND, true, continuation)
                        val tailUpdateToOffer = if (t.next.compareAndSet(null, offer)) {
                            this.tail.compareAndSet(t, offer)
                            true
                        } else {
                            this.tail.compareAndSet(t, t.next.value!!)
                            false
                        }
                        if (!tailUpdateToOffer) {
                            continuation.resume(false)
                            return@cont
                        }
                    }
                ) return
            } else {
                // dequeAndResume
                val node = dequeueForOperation(h)
                when (node.operationType) {
                    OperationType.RECEIVE -> {
                        node.x = element
                        node.continuation?.resume(true) ?: continue
                        return
                    }
                    OperationType.DUMP -> continue
                    else -> throw SynchronousQueueException()
                }
            }
        }
    }

    /**
     * Retrieves and removes an element from this channel if there is a waiting [send] invocation on it,
     * suspends the caller if this channel is empty.
     */
    suspend fun receive(): E {
        while (true) {
            val h = head.value
            val t = tail.value
            if (isEmpty(h, t) || t.operationType == OperationType.RECEIVE) { // we have some element to receive
                if (suspendCoroutine cont@{ continuation ->
                        val offer = Node<E>(null, OperationType.RECEIVE, false, continuation)
                        val tailUpdateToOffer = if (t.next.compareAndSet(null, offer)) {
                            this.tail.compareAndSet(t, offer)
                            true
                        } else {
                            this.tail.compareAndSet(t, t.next.value!!)
                            false
                        }
                        if (!tailUpdateToOffer) {
                            continuation.resume(false)
                            return@cont
                        }
                    }
                ) return t.next.value?.x ?: throw SynchronousQueueException()
            } else {
                val node = dequeueForOperation(h)
                when (node.operationType) {
                    OperationType.SEND -> {
                        node.continuation?.resume(true) ?: continue
                        return node.x ?: throw SynchronousQueueException()
                    }
                    OperationType.RECEIVE -> throw SynchronousQueueException()
                    else -> continue
                }
            }
        }
    }

    private fun isEmpty(head: Node<E>, tail: Node<E>): Boolean {
        return head == tail
    }
}

private class SynchronousQueueException : Exception() {
    override val message: String
        get() = "Got impossible operation type in node during send/receive retrieving from queue"
}

private class Node<E>(
    var x: E?,
    val operationType: OperationType = OperationType.DUMP,
    val isSend: Boolean? = null,
    val continuation: Continuation<Boolean>? = null
) {
    val next = atomic<Node<E>?>(null)

    fun isSender() = this.operationType == OperationType.SEND
}

private enum class OperationType {
    SEND,
    RECEIVE,
    DUMP
}
package mpp.faaqueue

import kotlinx.atomicfu.*

class FAAQueue<E> {
    private val head: AtomicRef<Segment> // Head pointer, similarly to the Michael-Scott queue (but the first node is _not_ sentinel)
    private val tail: AtomicRef<Segment> // Tail pointer, similarly to the Michael-Scott queue
    private val enqIdx = atomic(0L)
    private val deqIdx = atomic(0L)

    init {
        val firstNode = Segment()
        head = atomic(firstNode)
        tail = atomic(firstNode)
    }

    /**
     * Adds the specified element [x] to the queue.
     */
    fun enqueue(element: E) {
        while (true) {
            val curTail = tail.value
            val i = enqIdx.getAndAdd(1)
            val s = findSegment(curTail, i / SEGMENT_SIZE)
            if (!moveTailForward(curTail, s, i / SEGMENT_SIZE)) {
                continue
            }
            if (s.cas((i % SEGMENT_SIZE).toInt(), null, element)) {
                return
            }
        }
    }

    /**
     * Retrieves the first element from the queue and returns it;
     * returns `null` if the queue is empty.
     */
    fun dequeue(): E? {
        while (true) {
            val deq = deqIdx.value
            val enq = enqIdx.value
            if (deq >= enq) {
                return null
            }
            val curHead = head.value
            val i = deqIdx.getAndAdd(1)
            val s = findSegment(curHead, i / SEGMENT_SIZE)
            if (!moveHeadForward(curHead, s, i / SEGMENT_SIZE)) {
                continue
            }
            if (s.cas((i % SEGMENT_SIZE).toInt(), null, DUMP_ELEMENT)) {
                continue
            }
            return s.get((i % SEGMENT_SIZE).toInt()) as E?
        }
    }

    /**
     * Returns `true` if this queue is empty, or `false` otherwise.
     */
    val isEmpty: Boolean
        get() {
            return deqIdx.value >= enqIdx.value
        }

    private fun findSegment(segment: Segment, pos: Long): Segment {
        var newSegment = segment
        while (newSegment.id < pos) {
            val next = newSegment.next.value
            if (next == null) {
                val segmentToInsert = Segment(newSegment.id + 1)
                if (newSegment.next.compareAndSet(null, segmentToInsert)) {
                    newSegment = segmentToInsert
                    break
                }
            } else {
                newSegment = next
            }
        }
        return newSegment
    }

    private fun moveTailForward(curTail: Segment, s: Segment, pos: Long): Boolean {
        if (pos < s.id) {
            return tail.compareAndSet(curTail, s)
        }
        return true
    }

    private fun moveHeadForward(curHead: Segment, s: Segment,  pos: Long): Boolean {
        if (pos < s.id) {
            return head.compareAndSet(curHead, s)
        }
        return true
    }
}

private class Segment(
    val id: Long = 0L
) {
    val next: AtomicRef<Segment?> = atomic(null)
    val elements = atomicArrayOfNulls<Any>(SEGMENT_SIZE)

    fun get(i: Int) = elements[i].value
    fun cas(i: Int, expect: Any?, update: Any?) = elements[i].compareAndSet(expect, update)
    private fun put(i: Int, value: Any?) {
        elements[i].value = value
    }
}

const val SEGMENT_SIZE = 2 // DO NOT CHANGE, IMPORTANT FOR TESTS
const val DUMP_ELEMENT = "dump"


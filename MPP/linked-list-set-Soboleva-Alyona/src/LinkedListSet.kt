package mpp.linkedlistset

import kotlinx.atomicfu.atomic
import java.util.concurrent.atomic.AtomicMarkableReference

class LinkedListSet<E : Comparable<E>> {
    private val first = Node<E>(element = null)
    private val last = Node<E>(prev = first, element = null, next = null)

    init {
        first.setNext(last)
    }

    private val head = atomic(first)

    /**
     * Adds the specified element to this set
     * if it is not already present.
     *
     * Returns `true` if this set did not
     * already contain the specified element.
     */
    fun add(element: E): Boolean {
        while (true) {
            val window = findWindow(element)
            if (window.nextMarkable.reference?.element == element) {
                return false
            }
            val prev = window.prevMarkable.reference ?: throw IllegalArgumentException()
            val cur = window.nextMarkable.reference

            val node: Node<E> = Node(element)
            node.nextMarkable = AtomicMarkableReference(cur, false)
            if (prev.casNext(cur, node, expectedMark = false, updateMark = false)) {
                return true
            }

        }
    }

    /**
     * Removes the specified element from this set
     * if it is present.
     *
     * Returns `true` if this set contained
     * the specified element.
     */
    fun remove(element: E): Boolean {
        var snip: Boolean
        while (true) {
            val window = findWindow(element)
            if (window.nextMarkable.reference?.element != element) {
                return false
            }
            val cur = window.nextMarkable.reference
            val prev = window.prevMarkable.reference ?: throw IllegalArgumentException()
            return if (cur?.element != element) {
                false
            } else {
                val expectedReference = cur.nextMarkable.reference
                snip = cur.nextMarkable.attemptMark(expectedReference, true)
                if (!snip) continue
                prev.casNext(cur, expectedReference, false, false)
                true
            }
        }
    }

    /**
     * Returns `true` if this set contains
     * the specified element.
     */
    fun contains(element: E) = findWindow(element).nextMarkable.reference == element

    private fun findWindow(element: E): Node<E> {
        var pred: Node<E>?
        var curr: Node<E>?
        var expectedRef: Node<E>?
        val marked = booleanArrayOf(false)
        while (true) {
            pred = head.value
            curr = pred.nextMarkable.reference
            while (true) {
                expectedRef = curr!!.nextMarkable.get(marked)
                while (marked[0]) {
                    val successfullyCas = pred!!.casNext(curr, expectedRef, false, false)
                    if (!successfullyCas) {
                        break
                    }
                    curr = expectedRef
                    expectedRef = curr!!.nextMarkable.get(marked)
                }
                val res = Node(pred, null, curr)
                val curElement = curr?.element ?: return res
                if (curElement >= element) return res
                pred = curr
                curr = expectedRef
            }
        }
    }
}

class Node<E : Comparable<E>>(
    prev: Node<E>?,
    val element: E?,
    next: Node<E>?
) {

    constructor(element: E?) : this(null, element, null)

    fun setNext(value: Node<E>?) {
        nextMarkable.set(value, false)
    }

    fun casNext(expected: Node<E>?, update: Node<E>?, expectedMark: Boolean, updateMark: Boolean) =
        nextMarkable.compareAndSet(expected, update, expectedMark, updateMark)

    val prevMarkable = AtomicMarkableReference(prev, false)
    var nextMarkable = AtomicMarkableReference(next, false)
}
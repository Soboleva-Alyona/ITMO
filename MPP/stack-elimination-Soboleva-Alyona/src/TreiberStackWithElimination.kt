package mpp.stackWithElimination

import kotlinx.atomicfu.atomic
import kotlinx.atomicfu.atomicArrayOfNulls
import kotlin.random.Random.Default.nextInt

class TreiberStackWithElimination<E> {
    private val top = atomic<Node<E>?>(null)
    private val eliminationArray = atomicArrayOfNulls<Elimination<E?>?>(ELIMINATION_ARRAY_SIZE)

    /**
     * Adds the specified element [x] to the stack.
     */
    fun push(x: E) {
        if (tryEliminatePush(x)) {
            return
        }
        while (true) {
            val curTop = top.value
            val newTop = Node(x, curTop)
            if (top.compareAndSet(curTop, newTop)) {
                return
            }
        }
    }
    /**
     * Retrieves the first element from the stack
     * and returns it; returns `null` if the stack
     * is empty.
     */
    fun pop(): E? {
        tryEliminatePop()?.let { return it }
        while (true) {
            val curTop = top.value
            if (curTop?.x == null) {
                return null
            }

            val newTop = curTop.next
            if (top.compareAndSet(curTop, newTop)) {
                return curTop.x
            }
        }
    }

    private fun tryEliminatePush(x: E): Boolean {
        (0..ELIMINATION_ATTEMPTS).forEach { _ ->
            val posInEliminationArray = nextInt(ELIMINATION_ARRAY_SIZE)

            val eliminationToPushInto = Elimination<E?>(
                null,
                EliminationStatus.EMPTY
            )
            val xElimination = Elimination<E?>(
                x,
                EliminationStatus.OP_PUSH
            )
            if (eliminationArray[posInEliminationArray].compareAndSet(eliminationToPushInto, xElimination)) {
                if (waitPop(posInEliminationArray)) {
                    return true
                }
            }
        }
        return false
    }

    private fun waitPop(posInEliminationArray: Int): Boolean {
        val e = eliminationArray[posInEliminationArray]
        (0..WAITING_FOR_POP_TIME).forEach{ _ ->
            // just wait
        }
        val pushDoneElimination = Elimination<E?>(
            null,
            EliminationStatus.EMPTY
        )
        return e.value == pushDoneElimination
    }

    private fun tryEliminatePop(): E? {
        (0..ELIMINATION_ATTEMPTS).forEach { _ ->
            val posInEliminationArray = nextInt(ELIMINATION_ARRAY_SIZE)
            val eliminationToPop = eliminationArray[posInEliminationArray].value

            val emptyElimination = Elimination<E?>(
                null,
                EliminationStatus.EMPTY
            )

            if (eliminationToPop != null &&
                    eliminationArray[posInEliminationArray].compareAndSet(eliminationToPop, emptyElimination)) {
                return eliminationToPop.x
            }
        }
        return null
    }

    companion object {
        const val ELIMINATION_ATTEMPTS = 10
        const val WAITING_FOR_POP_TIME = 10
    }
}

private class Node<E>(val x: E, val next: Node<E>?)

private class Elimination<E>(val x: E?, val status: EliminationStatus)
private enum class EliminationStatus {
    EMPTY,
    OP_PUSH
}

private const val ELIMINATION_ARRAY_SIZE = 2 // DO NOT CHANGE IT
import kotlinx.atomicfu.atomic
import kotlinx.atomicfu.atomicArrayOfNulls
import java.util.*
import kotlin.random.Random.Default.nextInt

class FCPriorityQueue<E : Comparable<E>> {
    private val q = PriorityQueue<E>()
    private val threadsCnt = Runtime.getRuntime().availableProcessors()
    private val fcArray = CombiningArray<E>(threadsCnt)

    /**
     * Retrieves the element with the highest priority
     * and returns it as the result of this function;
     * returns `null` if the queue is empty.
     */
    fun poll(): E? {
        return putTaskToArrayAndWait(
            Task(
                Operation.POLL,
                null
            )
        )
    }

    /**
     * Returns the element with the highest priority
     * or `null` if the queue is empty.
     */
    fun peek(): E? {
        return putTaskToArrayAndWait(
            Task(
                Operation.PEEK,
                null
            )
        )
    }

    /**
     * Adds the specified element to the queue.
     */
    fun add(element: E) {
        putTaskToArrayAndWait(
            Task(
                Operation.ADD,
                element
            )
        )
    }

    private fun putTaskToArrayAndWait(task: Task<E>): E? {
        val taskInd = fcArray.put(task)
        while (true) {
            if (fcArray.tryLock()) {
                // we are combiner now
                for (i in 0 until fcArray.size) {
                    val cur = fcArray.get(i)
                    when (cur?.name) {
                        Operation.ADD -> q.add(cur.element)
                        Operation.POLL -> {
                            cur.element = q.poll()
                        }
                        Operation.PEEK -> {
                            cur.element = q.peek()
                        }
                        Operation.DONE -> {}
                        null -> {
                            continue
                        }
                    }

                    fcArray.markDone(i, cur)
                }
                fcArray.unlock()
            }
            val taskCondition = fcArray.get(taskInd) ?: continue
            if (taskCondition.name == Operation.DONE) {
                return fcArray.clearAndGet(taskInd)
            }
        }
    }

    private class CombiningArray<E>(val size: Int) {
        val lock = atomic(false)
        private val tasksList = atomicArrayOfNulls<Task<E>?>(size)

        fun tryLock(): Boolean {
            return lock.compareAndSet(false, true)
        }

        fun unlock() {
            lock.compareAndSet(true, false)
        }

        fun put(t: Task<E>): Int {
            while (true) {
                val ind = nextInt(size)
                if (tasksList[ind].compareAndSet(null, t)) {
                    return ind
                }
            }
        }

        fun get(i: Int): Task<E>? {
            return tasksList[i].value
        }

        fun clearAndGet(i: Int): E? {
            while (true) {
                val cur = tasksList[i].value
                if (tasksList[i].compareAndSet(cur, null)) {
                    return cur?.element
                }
            }
        }

        fun markDone(i: Int, curChanged: Task<E>) {
            val cur = tasksList[i].value ?: return
            tasksList[i].compareAndSet(curChanged, Task(
                    Operation.DONE,
                    cur.element
                )
            )
        }
    }

    private class Task<E>(
        var name: Operation, var element: E?
    )

    private enum class Operation {
        ADD, PEEK, POLL, DONE
    }
}
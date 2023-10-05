package dijkstra

import kotlinx.atomicfu.atomic
import java.util.*
import kotlin.random.Random.Default.nextInt

class MultiQueue(
    val capacity: Int,
    val comparator: Comparator<Node>
) {

    private var queuesList: List<PriorityQueue<Node>>
    private var queuesCnt = 0
    private val size = atomic(0)

    init {
        val tempList = mutableListOf<PriorityQueue<Node>>()
        queuesCnt = capacity * QUEUES_CNT_FACTOR
        (0 until queuesCnt).forEach { _ ->
            val q = PriorityQueue(capacity, comparator)
            tempList.add(q)
        }
        queuesList = tempList
    }

    fun add(node: Node) {
        val q = queuesList[nextInt(queuesCnt)]
        size.incrementAndGet()
        synchronized(q) {
            q.add(node)
        }
    }

    fun poll(): Node? {
        var ind1 = 0
        var ind2 = 0
        while (ind1 == ind2) {
            ind1 = nextInt(queuesCnt)
            ind2 = nextInt(queuesCnt)
        }

        val q1 = queuesList[ind1]
        val q2 = queuesList[ind2]

        synchronized(q1) {
            synchronized(q2) {
                return when (Pair(q1.isNotEmpty(), q2.isNotEmpty())) {
                    false to false -> null
                    true to false -> {
                        ind1
                    }
                    false to true -> {
                        ind2
                    }
                    else -> if (comparator.compare(q1.peek(), q2.peek()) < 0) {
                        ind1
                    } else {
                        ind2
                    }
                }?.let {
                    size.decrementAndGet()
                    queuesList[it].poll()
                }
            }
        }
    }

    fun isEmpty(): Boolean {
        return size.value == 0
    }

    companion object {
        const val QUEUES_CNT_FACTOR = 6
    }
}
package dijkstra

import java.util.concurrent.Phaser
import kotlin.Comparator
import kotlin.concurrent.thread

private val NODE_DISTANCE_COMPARATOR = Comparator<Node> { o1, o2 -> Integer.compare(o1!!.distance, o2!!.distance) }

// Returns `Integer.MAX_VALUE` if a path has not been found.
fun shortestPathParallel(start: Node) {
    val workers = Runtime.getRuntime().availableProcessors()
    // The distance to the start node is `0`
    start.distance = 0
    // Create a priority (by distance) queue and add the start node into it
    val q = MultiQueue(workers, NODE_DISTANCE_COMPARATOR)
    q.add(start)
    // Run worker threads and wait until the total work is done
    val onFinish = Phaser(workers + 1) // `arrive()` should be invoked at the end by each worker
    repeat(workers) {
        thread {
            while (true) {
                val u = synchronized(q) { q.poll() } ?: if (q.isEmpty()) break else continue
                for (v in u.outgoingEdges) {
                    while (true) {
                        val d = u.distance + v.weight
                        val toDistance = v.to.distance
                        if (d < toDistance) {
                            if (v.to.casDistance(toDistance, d)) {
                                q.add(v.to)
                                break
                            }
                        } else {
                            break
                        }
                    }
                }
            }
            onFinish.arrive()
        }
    }
    onFinish.arriveAndAwaitAdvance()
}
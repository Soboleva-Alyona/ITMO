package bfs.impl

import bfs.BFS
import data.Graph
import data.Point
import java.util.LinkedList

class SequentialBFS : BFS {

    override fun doBFS(graph: Graph, source: Point, destination: Point): IntArray {
        val d = Array(graph.countPointIndex(destination) + 1){ INF }

        val visited = Array(graph.countPointIndex(destination) + 1) {0}
        val sourceIndex = graph.countPointIndex(source)
        d[sourceIndex] = 0

        val queue = LinkedList<Point>()
        queue.addFirst(source)
        while (queue.isNotEmpty()) {
            val u = queue.removeFirst()
            graph.edgesFromVertex(u).forEach { v ->
                if (visited[graph.countPointIndex(v)] == 0) {
                    visited[graph.countPointIndex(v)] = 1
                    d[graph.countPointIndex(v)] = d[graph.countPointIndex(u)] + 1
                    queue.add(v)
                }
            }
        }

        return d.toIntArray()
    }

    companion object {
        private const val INF = Int.MAX_VALUE
    }

}

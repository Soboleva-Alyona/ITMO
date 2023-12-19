package bfs.impl

import bfs.BFS
import data.Graph
import data.Point
import java.util.LinkedList

class SequentialBFS : BFS {

    override fun doBFS(graph: Graph, source: Point, destination: Point): IntArray {
        val d = IntArray(graph.countPointIndex(destination) + 1) { INF }
        val visited = ArrayList<Int>(graph.countPointIndex(destination) + 1)
        visited.addAll((0 .. graph.countPointIndex(destination) + 1).map { 0 })
        val sourceIndex = graph.countPointIndex(source)
        d[sourceIndex] = 0

        val queue = LinkedList<Point>()
        queue.addFirst(source)
        while (queue.isNotEmpty()) {
            val u = queue.removeFirst()
            graph.edgesFromVertex(u).forEach { v ->
                val vIndex = graph.countPointIndex(v)
                if (visited[vIndex] == 0) {
                    visited[vIndex] = 1
                    d[vIndex] = d[graph.countPointIndex(u)] + 1
                    queue.add(v)
                }
            }
        }

        return d
    }

    companion object {
        private const val INF = Int.MAX_VALUE
    }

}

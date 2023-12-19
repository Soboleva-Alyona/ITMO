package bfs

import data.Graph
import data.Point

interface BFS {

    fun doBFS(graph: Graph, source: Point, destination: Point): IntArray

}

package bfs.impl

import bfs.BFS
import data.Graph
import data.Point
import utils.Fork2JoinUtils
import java.util.concurrent.ForkJoinPool
import java.util.concurrent.RecursiveAction
import java.util.concurrent.atomic.AtomicIntegerArray

class ParallelBFS(
    nThreads: Int = 4
) : BFS {

    private val threadPool = ForkJoinPool(nThreads)

    override fun doBFS(graph: Graph, source: Point, destination: Point): IntArray {
        var frontier = arrayOf(source)
        val visited = AtomicIntegerArray(graph.numberOfVertex())
        val graphDims = graph.sizes
        val d = IntArray(countPointIndex(graphDims, destination) + 1) { INF }
        val sourceIndex = countPointIndex(graphDims, source)
        d[sourceIndex] = 0

        while (frontier.isNotEmpty()) {
            val newFrontier = processFrontierInPFor(frontier, graph, graphDims, visited, d)

            frontier = newFrontier.filterNotNull().toTypedArray()
        }
        return d
    }

    private fun processFrontierInPFor(
        frontier: Array<Point>,
        graph: Graph,
        graphDims: IntArray,
        visited: AtomicIntegerArray,
        d: IntArray
    ): Array<Point?> {
        val degs = IntArray(frontier.size) { graph.edgesFromVertex(frontier[it]).size }
//        val degsLast = degs.last()
        val scannedDegs = parScan(degs)
//        val toCheck = seqScan(degs)
        val newFrontier = Array<Point?>(scannedDegs.last()) { null }

        threadPool.invoke(
            Fork2JoinUtils.PForAction(
                0,
                frontier.size - 1,
                { indexInFrontier ->
                    processVertexInFrontier(
                        frontier,
                        graph,
                        graphDims,
                        visited,
                        newFrontier,
                        scannedDegs,
                        indexInFrontier,
                        d
                    )
                }
            )
        )

        return newFrontier
    }

    private fun processVertexInFrontier(
        frontier: Array<Point>,
        graph: Graph,
        graphDims: IntArray,
        visited: AtomicIntegerArray,
        newFrontier: Array<Point?>,
        indexes: IntArray,
        indexInFrontier: Int,
        d: IntArray
    ) {
        val v = frontier[indexInFrontier]
        graph.edgesFromVertex(v).forEachIndexed { neighbourIndex, neighbour ->
//            println("Processing neighbour $neighbour of point $v")
            val neighbourInD = countPointIndex(graphDims, neighbour)
            if (visited.compareAndSet(neighbourInD, 0, 1)) {
                newFrontier[indexes[indexInFrontier] + neighbourIndex] = neighbour

                if (d[neighbourInD] == INF) {
                    d[neighbourInD] = d[countPointIndex(graphDims, v)] + 1
                }
            }
        }
    }

    fun shutdown() {
        threadPool.shutdown()
    }


    // exclusive
    private fun parScan(array: IntArray): IntArray {
        if (array.isEmpty()) {
            throw IllegalStateException("Empty array")
        }
        return Fork2JoinUtils.ParallelScan().scan(array)
    }

    private fun seqScan(array: IntArray) {
        var prevValue = 0
        var prevSum = 0
        for (i in array.indices) {
            val curValue = array[i]
            array[i] = prevSum + prevValue
            prevSum += prevValue
            prevValue = curValue
        }
    }

    private fun countPointIndex(dims: IntArray, point: Point): Int {
        var i = 0
        dims.zip(point.coords).forEach {
            i *= it.first
            i += it.second
        }
        return i
    }


    companion object {
        private const val BLOCK_SIZE = 10
        private const val INF = Int.MAX_VALUE
    }

    fun IntArray.print() {
        this.forEachIndexed { index, a ->
            print(a)
            if (index != this.size - 1) {
                print(",")
            }
            print(" ")
        }
        println()
    }

}

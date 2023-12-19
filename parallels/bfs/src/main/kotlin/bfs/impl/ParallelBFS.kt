package bfs.impl

import bfs.BFS
import data.Graph
import data.Point
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
        val degsLast = degs.last()
        seqScan(degs, 0, degs.size - 1)
        val newFrontier = Array<Point?>(degs.last() + degsLast) { null }

        threadPool.invoke(
            PForAction(
                0,
                frontier.size - 1,
                { indexInFrontier ->
                    processVertexInFrontier(
                        frontier,
                        graph,
                        graphDims,
                        visited,
                        newFrontier,
                        degs,
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

    private class PForAction(
        val l: Int,
        val r: Int,
        val lambda: (Int) -> Unit,
        val seqBlockSize: Int = 10
    ) : RecursiveAction() {

        override fun compute() {
            if (r - l < seqBlockSize) {
                (l..r).forEach {
                    lambda(it)
                }
                return
            }

            val m = (l + r) / 2;

            invokeAll(
                PForAction(l, m, lambda),
                PForAction(m + 1, r, lambda)
            )
        }
    }

    fun shutdown() {
        threadPool.shutdown()
    }


    // exclusive
    private fun seqScan(array: IntArray, l: Int, r: Int) {
        if (array.isEmpty()) {
            throw IllegalStateException("Empty array")
        }
        var prevValue = 0
        var prevSum = 0
        for (i in (l..r)) {
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

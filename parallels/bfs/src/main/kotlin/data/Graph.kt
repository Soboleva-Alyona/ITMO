package data

import java.util.function.Function

class Graph(
    val dim: Int,
    vararg val sizes: Int
) {
    // todo по факту sizes для произвольного графа не определено - нужно либо совсем убрать
    // todo либо сделать необязательным - нужно только когда определена edgeFunctions

    private var matrix: Array<*>? = null

    private var edgeFunctions: List<Function<Point, Point>>? = null

    fun setEdgeFunctions(list: List<Function<Point, Point>>) {
        edgeFunctions = list
    }

    fun numberOfVertex(): Int {
        return sizes.reduce { a, b -> a * b }
    }

    fun countPointIndex(point: Point): Int {
        var i = 0
        sizes.zip(point.coords).forEach {
            i *= it.first
            i += it.second
        }
        return i
    }

    fun setMatrix(matrix: Array<*>) {
        var currentLayer = matrix[0]

        var matrixDim = 0
        while (currentLayer is Array<*>) {
            currentLayer = currentLayer[0]
            matrixDim++
        }

        if (matrixDim != dim) {
            throw IllegalArgumentException("Matrix dim doesn't correspond to graph dim")
        }

        this.matrix = matrix
    }

    fun hasEdge(a: Point, b: Point): Boolean {
        if (a.dim != b.dim || a.dim != this.dim) {
            throw IllegalArgumentException("Points dim doesn't correspond to graph dim")
        }

        if (matrix == null && edgeFunctions == null) {
            return false
        }

        if (matrix == null) {
            edgeFunctions!!.forEach {
                val pointFromA = it.apply(a)
                if (pointFromA == b && isPointWithinGraph(pointFromA)) {
                    return true
                }
            }
            return false
        }

        // by matrix
        TODO()
        var currentLayer = matrix?.get(0)

        var matrixDim = 0
        while (currentLayer is Array<*>) {
            currentLayer = currentLayer[0]
            matrixDim++
        }

        return false
    }

    fun edgesFromVertex(u: Point): List<Point> {
        if (u.dim != this.dim) {
            throw IllegalArgumentException("Points dim doesn't correspond to graph dim")
        }

        if (matrix == null && edgeFunctions == null) {
            throw IllegalStateException("Graph edges wasn't initialized")
        }

        if (matrix != null) {
            return findEdgesFromUByMatrix(u)
        }

        return findEdgesFromUByEdgeFunctions(u)
    }

    private fun findEdgesFromUByMatrix(u: Point): MutableList<Point> {
        val edgesList = mutableListOf<Point>()



        return edgesList
    }

    private fun findEdgesFromUByEdgeFunctions(u: Point): MutableList<Point> {
        val edgesList = mutableListOf<Point>()
        edgeFunctions!!.forEach {
            val pointFromU = it.apply(u)
            if (isPointWithinGraph(pointFromU)) {
                edgesList.add(pointFromU)
            }
        }
        return edgesList
    }

    private fun isPointWithinGraph(a: Point): Boolean {
        (0 until a.dim).forEach {
            if (a.getNth(it) >= sizes[it]) {
                return false
            }
        }
        return true
    }

}

package data

import java.util.function.Function

class Graph(
    val dim: Int,
    vararg val sizes: Int
) {

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

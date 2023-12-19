import bfs.impl.ParallelBFS
import bfs.impl.SequentialBFS
import data.Graph
import data.Point
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import java.util.function.Function
import kotlin.system.measureTimeMillis

class BFSTest {

    @Test
    fun `should do bfs sequentially for cubic graph`() {
        // given
        val seq = SequentialBFS()

        val edgeFunctions = listOf<Function<Point, Point>>(
            Function { point -> Point(point.x + 1, point.y, point.z) },
            Function { point -> Point(point.x, point.y + 1, point.z) },
            Function { point -> Point(point.x, point.y, point.z + 1) }
        )

        val n = 100
        val graph = Graph(3, n, n, n)
        graph.setEdgeFunctions(edgeFunctions)

        val start = Point(0, 0, 0)
        val end = Point(n - 1, n - 1, n - 1)

        // when
        var d: IntArray
        val seqMs = measureTimeMillis {
            d = seq.doBFS(graph, start, end)
        }

        // then
        println("Sequential time: $seqMs ms")
        assertDistsAreValidInCubicGraph(d, graph)
    }

    @Test
    fun `should do bfs in parallel for cubic graph`() {
        // given
        val edgeFunctions = listOf<Function<Point, Point>>(
            Function { point -> Point(point.x + 1, point.y, point.z) },
            Function { point -> Point(point.x, point.y + 1, point.z) },
            Function { point -> Point(point.x, point.y, point.z + 1) }
        )

        val n = 100
        val graph = Graph(3, n, n, n)
        graph.setEdgeFunctions(edgeFunctions)
        val par = ParallelBFS()

        val start = Point(0, 0, 0)
        val end = Point(n - 1, n - 1, n - 1)

        // when
        var d: IntArray
        val parMs = measureTimeMillis {
            d = par.doBFS(graph, start, end)
        }

        // then
        println("Parallel time: $parMs ms")
        assertDistsAreValidInCubicGraph(d, graph)

    }

    private fun assertDistsAreValidInCubicGraph(d: IntArray, graph: Graph) {
        with(graph.sizes[0]) {
            (0 until this).forEach { s1 ->
                (0 until s1).forEach { s2 ->
                    (0 until s2).forEach { s3 ->
                        val u = Point(s1, s2, s3)
                        val uInd = graph.countPointIndex(u)
                        assertEquals(
                            u.coords.reduce { a, b -> a + b },
                            d[uInd],
                            "Incorrect d for point: $u with index $uInd"
                        )
                    }
                }
            }
        }
    }

}


import data.Graph
import data.Point
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import java.util.function.Function


class GraphTest {

    @Test
    fun `should return correct edges from start for cubic graph`() {
        // given
        val start = Point(0, 0, 0)

        val edgeFunctions = listOf<Function<Point, Point>>(
            Function { point -> Point(point.x + 1, point.y, point.z) },
            Function { point -> Point(point.x, point.y + 1, point.z) },
            Function { point -> Point(point.x, point.y, point.z + 1) }
        )
        val graph = Graph(3, 500, 500, 500)
        graph.setEdgeFunctions(edgeFunctions)

        // when
        val edges = graph.edgesFromVertex(start)

        // then
        assertTrue(edges.size == 3)
        assertTrue(edges.contains(Point(1, 0, 0)))
        assertTrue(edges.contains(Point(0, 1, 0)))
        assertTrue(edges.contains(Point(0, 0, 1)))
    }

    @Test
    fun `should return empty list of edges from finish for cubic graph`() {
        // given
        val start = Point(499, 499, 499)

        val edgeFunctions = listOf<java.util.function.Function<Point, Point>>(
            Function { point -> Point(point.x + 1, point.y, point.z) },
            Function { point -> Point(point.x, point.y + 1, point.z) },
            Function { point -> Point(point.x, point.y, point.z + 1) }
        )
        val graph = Graph(3, 500, 500, 500)
        graph.setEdgeFunctions(edgeFunctions)

        // when
        val edges = graph.edgesFromVertex(start)

        // then
        assertTrue(edges.isEmpty())
    }

}

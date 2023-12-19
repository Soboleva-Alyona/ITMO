import bfs.impl.ParallelBFS
import bfs.impl.SequentialBFS
import data.Graph
import data.Point
import java.util.function.Function
import kotlin.system.measureTimeMillis

fun main(args: Array<String>) {
    runNTimes(5, 500)
}


fun runNTimes(n: Int, size: Int = 500) {
    var seqSumTime = 0L
    var parSumTime = 0L

    val edgeFunctions = listOf<Function<Point, Point>>(
        Function { point -> Point(point.x + 1, point.y, point.z) },
        Function { point -> Point(point.x, point.y + 1, point.z) },
        Function { point -> Point(point.x, point.y, point.z + 1) }
    )

    val graph = Graph(3, size, size, size)
    graph.setEdgeFunctions(edgeFunctions)

    val start = Point(0, 0, 0)
    val end = Point(size - 1, size - 1, size - 1)

    (1..n).forEach {
        val seq = SequentialBFS()
        val par = ParallelBFS()

        val seqMs = measureTimeMillis {
            seq.doBFS(graph, start, end)
        }
        println("Sequential time: $seqMs ms")

        // try par
        val parMs = measureTimeMillis {
            par.doBFS(graph, start, end)
        }
        par.shutdown()
        println("Parallel time: $parMs ms")
        parSumTime += parMs
        seqSumTime += seqMs
        println("---------------------------------------")
    }
    println("Average sequential time: ${seqSumTime / n} ms VS Average parallel time: ${parSumTime / n} ms")
    val coeff = seqSumTime.div(parSumTime.toFloat())
    println("Parallel is $coeff faster then sequential")
}
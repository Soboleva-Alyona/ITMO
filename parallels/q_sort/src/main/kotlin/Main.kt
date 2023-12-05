import impl.ParQuickSort
import impl.SeqQuickSort
import java.util.*
import kotlin.collections.ArrayList
import kotlin.system.measureTimeMillis

fun main() {
    // test seq
    val seq = SeqQuickSort()
    val array = generateArray(10, 1000)
    val arrayPar = ArrayList(array)

    println(array)
    seq.sort(array)
    println(array)

    // test par
    val par = ParQuickSort()
    println(arrayPar)
    par.sort(arrayPar)
    println(arrayPar)

    // benchmark
    runNTimes(5)
}

fun runNTimes(n: Int, size: Int = 1e8.toInt()) {
    var seqSumTime = 0L
    var parSumTime = 0L

    val seqQuickSort = SeqQuickSort()
    val parQuickSort = ParQuickSort()

    (1..n).forEach {
        println("Run number $it for array of size=$size")
        val arrayForSeqSort = generateArray(size = size)
        val arrayForParSort = ArrayList(arrayForSeqSort)

        val seqTime = measureTimeMillis {
            seqQuickSort.sort(arrayForSeqSort)
        }
        println("Sequential time: $seqTime ms")
        seqSumTime += seqTime


        val parTime = measureTimeMillis {
            parQuickSort.sort(arrayForParSort)
        }
        println("Parallel time: $parTime ms")
        parSumTime += parTime
        println("---------------------------------------")
    }
    println("Average sequential time: ${seqSumTime  / n} ms VS Average parallel time: ${parSumTime  / n} ms")
    val coeff = seqSumTime.div(parSumTime.toFloat())
    println("Parallel is $coeff faster then sequential")

}

fun generateArray(size: Int = 1e8.toInt(), bound: Int = Int.MAX_VALUE): ArrayList<Int> {
    val rand = Random()
    return ArrayList((1..size).map { rand.nextInt(bound) })
}

import impl.ParQuickSort
import impl.SeqQuickSort
import java.util.*
import kotlin.system.measureTimeMillis

fun main() {
    // test seq
    val seq = SeqQuickSort()
    val array = generateArray(10, 1000)
    val arrayPar = generateArray(10, 1000)

    array.print()
    seq.sort(array)
    array.print()

    // test par
    val par = ParQuickSort()
    arrayPar.print()
    par.sort(arrayPar)
    arrayPar.print()

    // benchmark
    runNTimes(5, 1e8.toInt())
}

fun runNTimes(n: Int, size: Int = 1e8.toInt()) {
    var seqSumTime = 0L
    var parSumTime = 0L

    (1..n).forEach {
        val seqQuickSort = SeqQuickSort()
        val parQuickSort = ParQuickSort()

        println("Run number $it for array of size=$size")
        val arrayForSeqSort = generateArray(size = size)
        val arrayForParSort = IntArray(size) { i -> (arrayForSeqSort[i]) }

        val seqTime = measureTimeMillis {
            seqQuickSort.sort(arrayForSeqSort)
        }
        println("Sequential time: $seqTime ms")
        seqSumTime += seqTime


        val parTime = measureTimeMillis {
            parQuickSort.sort(arrayForParSort)
        }
        parQuickSort.shutdown()
        println("Parallel time: $parTime ms")
        parSumTime += parTime
        println("---------------------------------------")
    }
    println("Average sequential time: ${seqSumTime / n} ms VS Average parallel time: ${parSumTime / n} ms")
    val coeff = seqSumTime.div(parSumTime.toFloat())
    println("Parallel is $coeff faster then sequential")
}

fun generateArray(size: Int = 1e8.toInt(), bound: Int = Int.MAX_VALUE): IntArray {
    val rand = Random()
    return IntArray(size) { rand.nextInt(bound) }
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
package impl

import QuickSort
import java.util.*
import java.util.concurrent.ForkJoinPool
import java.util.concurrent.RecursiveTask

class ParQuickSort(
    private val seqBlockSize:Int = 1000,
    nThreads: Int = 4
) : QuickSort {

    private val threadPool = ForkJoinPool(nThreads)

    override fun sort(array: ArrayList<Int>): ArrayList<Int> {
        threadPool.invoke(
            SortTask(array, 0, array.size - 1, seqBlockSize)
        )
        threadPool.shutdown()
        return array
    }

    private class SortTask(
        val array: ArrayList<Int>,
        val l: Int,
        val r: Int,
        val seqBlockSize: Int
    ) : RecursiveTask<Unit>() {

        private val rand = Random()

        private val seqSort = SeqQuickSort()

        override fun compute() {
            if (l < r) {
                if (r - l <= seqBlockSize) {
                    seqSort.quickSortInterval(array, l, r)
                    return
                }
                val m = partition(array, l, r)

                invokeAll(
                    SortTask(array, l, m, seqBlockSize),
                    SortTask(array, m + 1, r, seqBlockSize)
                )
            }
        }

        private fun partition(array: ArrayList<Int>, l: Int, r: Int): Int {
            val m = rand.nextInt(l, r)
            val value = array[m]

            var i = l
            var j = r
            while (i <= j) {
                while (array[i] < value) {
                    i++
                }
                while (array[j] > value) {
                    j--
                }
                if (i >= j) {
                    break
                }
                swap(array, i, j)
                i++
                j--

            }
            return j
        }

        private fun swap(array: ArrayList<Int>, first: Int, second: Int) {
            val firstVal = array[first]
            array[first] = array[second]
            array[second] = firstVal
        }

    }
}
package impl

import QuickSort
import java.util.*

class SeqQuickSort : QuickSort {

    private val rand = Random()

    override fun sort(array: ArrayList<Int>): ArrayList<Int> {
        quickSortInterval(array, 0, array.size - 1)
        return array
    }

    fun quickSortInterval(array: ArrayList<Int>, l: Int, r: Int) {
        if (l < r) {
            val m = partition(array, l, r)
            quickSortInterval(array, l, m)
            quickSortInterval(array, m + 1, r)
        }
    }

    private fun partition(array: ArrayList<Int>, l: Int, r: Int): Int {
        val m = rand.nextInt(l, r + 1)
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
package utils

import java.util.concurrent.ForkJoinPool
import java.util.concurrent.RecursiveAction
import kotlin.math.*

class Fork2JoinUtils {

    companion object {
        private const val nThreads: Int = 4
        private val threadPool = ForkJoinPool(nThreads)
    }

    // parallel scan implementation
    class ParallelScan {

        lateinit var binTree: IntArray

        lateinit var sums: IntArray

        companion object {
            val seqBlockSize = 10_000
        }

        private fun init(array: IntArray) {
            val height = log2(array.size.toDouble()).nextUp().toInt() + 1
            val treeSize = 2.0.pow(height).toInt() + 2
            binTree = IntArray(treeSize)
            sums = IntArray(array.size) + 1
        }

        fun scan(array: IntArray): IntArray {
            init(array)
            threadPool.invoke(
                UpAction(array, 0, array.size - 1, 1, binTree)
            )

            threadPool.invoke(
                DownAction(sums, array, 0, array.size - 1, 1, binTree)
            )
            return sums
        }

        class DownAction(
            private val sums: IntArray,
            private val array: IntArray,
            private val l: Int,
            private val r: Int,
            private val indexInTree: Int,
            private val binTree: IntArray,
            private val leftPrefix: Int = 0,
            private val seqBlockSize: Int = ParallelScan.seqBlockSize
        ): RecursiveAction() {

            override fun compute() {
                if (r - l < seqBlockSize) {
                    seqScan()
                    return
                }

                val m = (l + r) / 2

                invokeAll(
                    DownAction(sums, array, l, m, indexInTree * 2, binTree, leftPrefix),
                    DownAction(
                        sums,
                        array,
                        m + 1,
                        r,
                        indexInTree * 2 + 1,
                        binTree,
                        leftPrefix + binTree[indexInTree * 2])
                )
            }

            private fun seqScan() {
                sums[l + 1] = array[l] + leftPrefix
                for (i in (l + 1 .. r)) {
                    sums[i + 1] = sums[i] + array[i]
                }
            }
        }

        class UpAction(
            private val array: IntArray,
            private val l: Int,
            private val r: Int,
            private val indexInTree: Int,
            private val binTree: IntArray,
            private val seqBlockSize: Int = ParallelScan.seqBlockSize
        ): RecursiveAction() {
            override fun compute() {
                if (r - l < seqBlockSize) {
                    computeSumsSeq()
                    return
                }

                val m = (l + r) / 2

                invokeAll(
                    UpAction(array, l, m, indexInTree * 2, binTree),
                    UpAction(array, m + 1, r, indexInTree * 2 + 1, binTree)
                )
                binTree[indexInTree] = binTree[indexInTree * 2] + binTree[indexInTree * 2 + 1]
            }

            private fun computeSumsSeq() {
                var sum = 0
                for (i in (l..r)) {
                    sum += array[i]
                }
                binTree[indexInTree] = sum
            }
        }

    }


    // parallel for implementation
    class PForAction(
        private val l: Int,
        private val r: Int,
        private val lambda: (Int) -> Unit,
        private val seqBlockSize: Int = 1000
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

}
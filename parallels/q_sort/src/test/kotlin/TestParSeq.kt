import impl.ParQuickSort
import impl.SeqQuickSort
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import java.util.*
import kotlin.collections.ArrayList

internal class TestParSeq {

    private val seqQuickSort = SeqQuickSort()

    private val parQuickSort = ParQuickSort()

    @Test
    fun testSeqSort() {
        // given
        val generatedArray = generateArray()
        val sorted = ArrayList(generatedArray)
        sorted.sort()

        // when
        seqQuickSort.sort(generatedArray)

        // then
        assertEquals(sorted, generatedArray)
    }

    @Test
    fun testParSort() {
        // given
        val generatedArray = generateArray()
        val sorted = ArrayList(generatedArray)
        sorted.sort()

        // when
        parQuickSort.sort(generatedArray)

        // then
        assertEquals(sorted, generatedArray)
    }

    private fun generateArray(size: Int = 1e6.toInt()): ArrayList<Int> {
        val rand = Random()
        return ArrayList((1 .. size).map { rand.nextInt() })
    }

}
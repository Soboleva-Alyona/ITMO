/**
 * В теле класса решения разрешено использовать только переменные делегированные в класс RegularInt.
 * Нельзя volatile, нельзя другие типы, нельзя блокировки, нельзя лазить в глобальные переменные.
 *
 * @author Soboleva Elena
 */
class Solution : MonotonicClock {
    private var c1_1 by RegularInt(0)
    private var c1_2 by RegularInt(0)
    private var c1_3 by RegularInt(0)

    private var c2_1 by RegularInt(0)
    private var c2_2 by RegularInt(0)
    private var c2_3 by RegularInt(0)

    override fun write(time: Time) {
        // write left-to-right
        c2_1 = time.d1
        c2_2 = time.d2
        c2_3 = time.d3

        // write right-to-left
        c1_3 = c2_3
        c1_2 = c2_2
        c1_1 = c2_1
    }

    override fun read(): Time {
        // read left-to-right
        val t1_1 = c1_1
        val t1_2 = c1_2
        val t1_3 = c1_3
        val t1 = Time(t1_1, t1_2, t1_3)

        // read right-to-left
        val t2_3 = c2_3
        val t2_2 = c2_2
        val t2_1 = c2_1
        val t2 = Time(t2_1, t2_2, t2_3)

        if (t1 == t2) {
            return t1
        }
        var p = 0
        if (t1.d1 == t2.d1 && t1.d2 == t2.d2) {
            p = 2
        } else if (t1.d1 == t2.d1) {
            p = 1
        }

        return when (p) {
            0 -> Time(t2.d1, 0, 0)
            1 -> Time(t2.d1, t2.d2, 0)
            2 -> Time(t2.d1, t2.d2, t2.d3)
            else -> Time(0, 0, 0) // impossible case
        }
    }
}
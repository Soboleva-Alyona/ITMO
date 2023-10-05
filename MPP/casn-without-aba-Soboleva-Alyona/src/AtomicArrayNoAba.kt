import kotlinx.atomicfu.atomic
import kotlinx.atomicfu.loop

class AtomicArrayNoAba<E>(size: Int, initialValue: E) {
    private val arr = Array(size) { Ref(initialValue) }

    fun get(index: Int) = arr[index].value

    fun set(index: Int, value: E) {
        arr[index].value = value
    }

    fun cas(index: Int, expected: E, update: E) = arr[index].cas(expected, update)

    fun cas2(
        index1: Int, expected1: E, update1: E, index2: Int, expected2: E, update2: E
    ): Boolean {
        if (index1 == index2) {
            return if (expected1 == expected2) cas(index1, expected1, ((expected1 as Int) + 2) as E)
            else false
        }
        val minInd = if (index1 < index2) index1 else index2
        val maxInd = if (index1 > index2) index1 else index2
        val a: Ref<E> = arr[minInd]
        val b: Ref<E> = arr[maxInd]

        val expA: E = if (index1 < index2) expected1 else expected2
        val updA: E = if (index1 < index2) update1 else update2
        val expB: E = if (index1 < index2) expected2 else expected1
        val updB: E = if (index1 < index2) update2 else update1


        val caS2Descriptor = CAS2Descriptor(a, expA, updA, b, expB, updB)
        return if (a.cas(expA, caS2Descriptor)) caS2Descriptor.complete() else false
    }
}

class Ref<T>(initial: T) {
    val v = atomic<Any?>(initial)

    var value: T
        get() {
            v.loop { cur ->
                when (cur) {
                    is Descriptor -> cur.complete()
                    else -> return cur as T
                }
            }
        }
        set(upd) {
            v.loop { cur ->
                when (cur) {
                    is Descriptor -> cur.complete()
                    else -> if (v.compareAndSet(cur, upd)) return
                }
            }
        }

    fun cas(expect: Any?, update: Any?): Boolean {
        v.loop { current ->
            if (current is Descriptor) current.complete()
            else if (expect != current) return false
            else if (v.compareAndSet(current, update)) return true
        }
    }
}

private abstract class Descriptor {
    abstract fun complete(): Boolean
}

private class RDCSSDescriptor(
    val a: Ref<out Any?>, val expectA: Any?, val updateA: Any?,
    val b: Ref<out Any?>, val expectB: Any?,
) : Descriptor() {
    val outcome = atomic<Boolean?>(null)

    override fun complete(): Boolean {
        val upd = if (b.value == expectB) updateA else expectA
        outcome.compareAndSet(null, b.value == expectB)
        val outcomeValue = outcome.value!!
        a.v.compareAndSet(this, upd)
        return outcomeValue
    }
}

fun dcssMod(
    a: Ref<out Any?>, expectA: Any?, updateA: Any?, b: Ref<out Any?>, expectB: Any?
): Boolean {
    val desc = RDCSSDescriptor(a, expectA, updateA, b, expectB)
    return if (!a.cas(expectA, desc)) false
    else desc.complete()
}

private class CAS2Descriptor<T>(
    val a: Ref<T>, val expectA: T, val updateA: T,
    val b: Ref<T>, val expectB: T, val updateB: T,
) : Descriptor() {
    val outcome: Ref<Boolean?> = Ref(null)

    override fun complete(): Boolean {
        if (b.v.value != this) dcssMod(b, expectB, this, outcome, null)

        outcome.v.compareAndSet(null, b.v.value == this)

        val outcomeValue = outcome.value!!

        val aValue = if (outcomeValue) updateA else expectA
        a.v.compareAndSet(this, aValue)

        val bValue = if (outcomeValue) updateB else expectB
        b.v.compareAndSet(this, bValue)

        return outcomeValue
    }
}
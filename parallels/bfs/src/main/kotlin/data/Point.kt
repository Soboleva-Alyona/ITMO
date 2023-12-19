package data

class Point(vararg val coords: Int) {

    val dim = coords.size

    val x = getNth(0)

    val y = getNth(1)

    val z = getNth(2)

    fun getNth(n: Int): Int {
        assert(n in 0 until  dim)
        return coords[n]
    }


    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as Point

        if (dim != other.dim) return false

        if (!coords.contentEquals(other.coords)) return false

        return true
    }

    override fun hashCode(): Int {
        var result = coords.contentHashCode()
        result = 31 * result + dim
        return result
    }

    override fun toString(): String {
        var coordsString = ""
        coords.forEach {
            coordsString = coordsString.plus(it)
            coordsString = coordsString.plus(", ")
        }
        return "Point(${coordsString.substring(0, coordsString.length - 2)})"
    }

}
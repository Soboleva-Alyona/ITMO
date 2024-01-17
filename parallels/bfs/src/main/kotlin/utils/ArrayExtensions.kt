package utils

//fun IntArray.parallelMap(f: (Int) -> Int) {
//    Fork2JoinUtils.PForAction(
//        0,
//        this.size,
//        { index -> this[index] = f(this[index]) }
//    )
//}

fun IntArray.parallelScan(): IntArray {
    return Fork2JoinUtils.ParallelScan().scan(this)
}


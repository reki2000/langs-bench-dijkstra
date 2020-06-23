class FastIO {
    private val input = System.`in`
    private val buffer = ByteArray(1024)
    private var pointer = 0
    private var bufferLength = 0
    private val pw = java.io.PrintWriter(System.out)
    private fun Byte.isPrintable() = this in 46..126
    private fun Byte.isNumeric() = this in '0'.toByte()..'9'.toByte()
    private fun Byte.toNumVal() =
        if (this.isNumeric()) this - '0'.toByte() else error("$this is not numeric")

    private fun hasNextByte(): Boolean {
        return if (pointer < bufferLength) true else {
            pointer = 0
            bufferLength = input.read(buffer)
            bufferLength > 0
        }
    }

    private fun readByte(): Byte = if (hasNextByte()) buffer[pointer++] else -1
    private fun skipUnprintable() = run { while (hasNextByte() && !buffer[pointer].isPrintable()) pointer++ }
    fun hasNext(): Boolean = run { skipUnprintable() }.run { hasNextByte() }
    private fun hasNextOrError() = if (!hasNext()) error("has no next element.") else Unit

    fun readString(): String {
        hasNextOrError()
        val sb = StringBuilder()
        var b = readByte()
        while (b.isPrintable()) {
            sb.appendCodePoint(b.toInt())
            b = readByte()
        }
        return sb.toString()
    }

    fun readLong(): Long {
        hasNextOrError()
        var n = 0L
        var negative = false
        var b = readByte()
        if (b == '-'.toByte()) {
            negative = true
            b = readByte()
        }
        if (!b.isNumeric()) error("$b is not numeric.")
        while (true) {
            when {
                b.isNumeric() -> n = n * 10 + b.toNumVal()
                b.toInt() == -1 || !b.isPrintable() -> return if (negative) -n else n
                else -> error("failed to parse. [n=$n, b=$b]")
            }
            b = readByte()
        }
    }

    fun readInt() = readLong()
        .let { if (it in Int.MIN_VALUE..Int.MAX_VALUE) it.toInt() else error("$it is not in range of Int.") }

    fun readDouble(): Double {
        var n = 0.0
        var div = 1.0
        var negative = false
        var b = readByte()
        if (b == '-'.toByte()) {
            negative = true
            b = readByte()
        }
        do n = n * 10 + b.toNumVal()
        while (run { b = readByte() }.run { b.isNumeric() })
        if (b == '.'.toByte()) {
            while (run { b = readByte() }.run { b.isNumeric() })
                n += b.toNumVal() / (run { div *= 10 }.run { div })
        }
        return if (negative) -n else n
    }

    fun readStringList(size: Int) = readList(size) { readString() }
    fun readIntList(size: Int) = readList(size) { readInt() }
    fun readLongList(size: Int) = readList(size) { readLong() }
    fun readDoubleList(size: Int) = readList(size) { readDouble() }
    fun readIntArray(size: Int) = IntArray(size) { readInt() }
    fun readLongArray(size: Int) = LongArray(size) { readLong() }
    fun readDoubleArray(size: Int) = DoubleArray(size) { readDouble() }
    inline fun <reified T> readArray(size: Int, init: () -> T) = Array(size) { init() }
    inline fun <T> readList(size: Int, init: () -> T) = List(size) { init() }
    fun println() = pw.println()
    fun print(o: Any?) = pw.print(o)
    fun println(o: Any?) = pw.println(o)
    fun flush() = pw.flush()
}

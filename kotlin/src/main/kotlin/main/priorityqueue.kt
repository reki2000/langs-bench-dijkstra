package main

typealias P = Pair<Int,Int>

class PriorityQueue() {
	val tree: MutableList<P> = mutableListOf<P>()

	private fun swap(i: Int, j: Int) {
		val tmp = tree[j]
		tree[j] = tree[i]
		tree[i] = tmp
	}

	private fun _isPrior(i: Int, j: Int): Boolean {
		return tree[i].first < tree[j].first
	}
	
	fun empty(): Boolean = tree.isEmpty()

	fun push(v: P) {
		var index = tree.size
		tree.add(v)
		while (index > 0) {
			val parentIndex = ((index + 1) shr 1) - 1
			if (_isPrior(index, parentIndex)) {
				swap(index, parentIndex)
				index = parentIndex
			} else {
				break
			}
		}
	}

	fun pop(): P {
		val result = tree[0]
		val last = tree.last()
		val size = tree.size - 1
		tree.removeAt(size)
		if (size == 0) {
			return last
		}
		tree[0] = last
		var index = 0
		while (true) {
			var child = ((index + 1) shl 1) - 1
			if (child >= size) {
				break
			}
			val rIndex = child + 1
			if (rIndex < size && _isPrior(rIndex, child)) {
				child = rIndex
			}
			if (_isPrior(index, child)) {
				break
			}
			swap(index, child)
			index = child
		}
		return result
	}
}

package main

import java.io.PrintWriter

typealias Edge = Pair<NodeIndex, Distance>

typealias NodeId = Int
typealias NodeIndex = Int
typealias Distance = Int

const val DISTANCE_MULTIPLE = 100

var is_debug = false

val pw = PrintWriter(System.out)

class G(
	var id2idx: MutableMap<NodeId, NodeIndex> = mutableMapOf<NodeId, NodeIndex>(),
	var idx2id: MutableList<NodeId> = mutableListOf(0),
	var idx: NodeIndex = 1,
	var edge: MutableList<MutableList<Edge>> = mutableListOf(mutableListOf<Edge>())
)

val g = G()

fun get_idx(id: NodeId): NodeIndex {
	return g.id2idx.getOrElse(id, {
		val i = g.idx++
		g.id2idx[id] = i
		g.idx2id.add(id)
		g.edge.add(mutableListOf<Edge>())
		return i
	})
}


fun add_edge(start: NodeId, end: NodeId, distance: Distance) {
	val s = get_idx(start)
	val e = get_idx(end)
	g.edge[s].add(Edge(e, distance))
}

fun stof100(s: String): Int {
	var result = 0
	var place = 2
	var is_decimal_place = false
	for (ch in s) {
		if (ch == '.') {
			is_decimal_place = true
			continue
		}
		result *= 10
		result += ch - '0'
		if (is_decimal_place) {
			place--
			if (place == 0) {
				break
			}
		}
	}
	while (place > 0) {
		result *= 10
		place--
	}
	return result
}

fun load() {
	System.`in`.bufferedReader().use { reader ->
		reader.readLine() // skip header

		while (true) {
			val line = reader.readLine() ?: break
			val fields = line.split(',')
			val s = Integer.valueOf(fields[2])
			val e = Integer.valueOf(fields[3])
			val d = stof100(fields[5])
			if (is_debug) pw.println("line: $line s: $s e: $e D: $d")
			add_edge(s, e, d)
		}
	}
}

typealias Visit = Pair<Distance, NodeIndex>
typealias Result = Pair<Distance, List<NodeId>>

fun dijkstra(start: NodeId, end: NodeId): Result {
	val s = get_idx(start)
	val e = get_idx(end)

	val size = g.idx
	val d = Array<Distance>(size) { Int.MAX_VALUE }
	val prev = Array<NodeIndex>(size) { 0 }

	val queue = PriorityQueue()
	queue.push(Visit(0, s))

	var visited = 0
	while (!queue.empty()) {
		val (distance, here) = queue.pop()
		if (distance > d[here]) {
			continue
		}
		visited++
		if (is_debug) pw.println("visiting: $here distance: $distance")

		for ((to, weight) in g.edge[here]) {
			val w = distance + weight
			if (w < d[to]) {
				prev[to] = here
				d[to] = w
				queue.push(Visit(w, to))
			}
		}
	}
	pw.println("visited: $visited")

	var n = e
	val result = mutableListOf(g.idx2id[n])

	while (d[n] != Int.MAX_VALUE && n != s && n != 0) {
		n = prev[n]
		result.add(g.idx2id[n])
	}

	return Result(d[e] / DISTANCE_MULTIPLE, result)
}

fun main(args: Array<String>) {
	val count = args[0].toInt()
	is_debug = args.size > 1 && args[1] == "debug"

	pw.use {
		load()
		pw.println("loaded nodes: ${g.idx}")

		var distance: Distance
		var route = listOf<NodeId>()
		for (i in 1..count) {
			val s = g.idx2id[i * 1000]
			val result = dijkstra(s, g.idx2id[1])
			distance = result.first
			route = result.second
			pw.println("distance: $distance")
		}

		pw.println("route: ${route.joinToString(separator = " ", postfix = " ")}")
		pw.flush()
	}
}

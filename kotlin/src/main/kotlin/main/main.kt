package main

typealias Edge = Pair<NodeIndex, Distance>

typealias NodeId = Int
typealias NodeIndex = Int
typealias Distance = Int

const val DISTANCE_MULTIPLE = 100

var is_debug = false

val io = FastIO()

class G(
	var id2idx:MutableMap<NodeId,NodeIndex> = mutableMapOf<NodeId,NodeIndex>(),
	var idx2id:MutableList<NodeId> = mutableListOf(0),
	var idx:NodeIndex = 1,
	var edge:MutableList<MutableList<Edge>> = mutableListOf(mutableListOf<Edge>())
)

val g = G()

fun get_idx(id:NodeId): NodeIndex {
	return g.id2idx.getOrElse(id, {
		val i = g.idx++
		g.id2idx[id] = i
		g.idx2id.add(id) 
		g.edge.add(mutableListOf<Edge>())
		return i
	})
}


fun add_edge(start:NodeId, end:NodeId, distance:Distance) {
	val s = get_idx(start)
	val e = get_idx(end)
	g.edge[s].add(Edge(e, distance))
}

fun stof100(s:String):Int {
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
				break;
			}
		}
	}
	while (place > 0) {
		result *= 10
		place--
	}
	return result;
}

fun load() {
	io.readStringList(6) // skip header

	while (io.hasNext()) {
		io.readString() // skip 1st elem
		io.readString() // skip 2nd elem
		val s = io.readInt()
		val e = io.readInt()
		io.readString() // skip 5th elem
		val d = stof100(io.readString())
		if (is_debug) io.println("s: $s e: $e D: $d")
		add_edge(s, e, d)
	}
}

typealias Visit = Pair<Distance, NodeIndex>
typealias Result = Pair<Distance, List<NodeId>>

fun dijkstra(start:NodeId, end:NodeId):Result {
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
		if (is_debug) io.println("visiting: $here distance: $distance")

		for ((to, weight) in g.edge[here]) {
			val w = distance + weight
			if (w < d[to]) {
				prev[to] = here
				d[to] = w
				queue.push(Visit(w, to))
			}
		}
	}
	io.println("visited: $visited")

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

	load()
	io.println("loaded nodes: " + g.idx)

	var distance: Distance
	var route = listOf<NodeId>()
	for (i in 1..count) {
		val s = g.idx2id[i*1000]
		val result = dijkstra(s, g.idx2id[1])
		distance = result.first
		route = result.second
		io.println("distance: $distance")
	}

	io.print("route: ")
	for (id in route) {
		io.print("$id ")
	}
	io.println()
	io.flush()
}

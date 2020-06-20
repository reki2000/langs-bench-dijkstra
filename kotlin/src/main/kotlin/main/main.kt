package main

typealias Edge = Pair<NodeIndex, Distance>

typealias NodeId = Int
typealias NodeIndex = Int
typealias Distance = Int

val DISTANCE_MULTIPLE = 100

var is_debug = false

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
	var place = 0
	for (ch in s) {
		if (ch == '.') {
			place = 1
			continue
		}
		result *= 10
		result += ch - '0'
		if (place > 0) {
			place++
			if (place >= 3) {
				break;
			}
		}
	}
	while (place < 3) {
		result *= 10
		place++
	}
	return result;
}

fun load() {
	readLine() // skip header

	while (true) {
		val line = readLine()
		if (line == null) {
			break
		}
		val fields = line.split(",")
		val s = fields[2].toInt()
		val e = fields[3].toInt()
		val d = stof100(fields[5])
		if (is_debug) println("line: " + line + " s: " + s + " e: " + e + " D: " + d)
		add_edge(s, e, d)
	}
}

typealias Visit = Pair<Distance, NodeIndex>
typealias Result = Pair<Distance, List<NodeId>>

fun dijkstra(start:NodeId, end:NodeId):Result {
	val s = get_idx(start)
	val e = get_idx(end)

	val size = g.idx
	val d = Array<Distance>(size) { 0 }
	val prev = Array<NodeIndex>(size) { 0 }

	val queue = PriorityQueue()
	queue.push(Visit(0, s))

	var visited = 0
	while (!queue.empty()) {
		val (distance, here) = queue.pop()
		visited++
		if (is_debug) println("visiting: " + here + " distance: " + distance)

		for ((to, weight) in g.edge[here]) {
			val w = distance + weight
			if (d[to] == 0 || w < d[to]) {
				prev[to] = here
				d[to] = w
				queue.push(Visit(w, to))
			}
		}
	}
	println("visited: "+ visited)

	var n = e
	val result = mutableListOf(g.idx2id[n])

	while (d[n] != 0 && n != s && n != 0) {
		n = prev[n]
		result.add(g.idx2id[n])
	}

	return Result(d[e] / DISTANCE_MULTIPLE, result)
}

fun main(args: Array<String>) {
	val count = args[0].toInt() 
	is_debug = args.size > 1 && args[1] == "debug"

	load()
	println("loaded nodes: " + g.idx)

	var distance: Distance
	var route = listOf<NodeId>()
	for (i in 1..count) {
		val s = g.idx2id[i*1000]
		val result = dijkstra(s, g.idx2id[1])
		distance = result.first
		route = result.second
		println("distance: " + distance)
	}

	print("route: ")
	for (id in route) {
		print("" + id + " ")
	}
	println()
}

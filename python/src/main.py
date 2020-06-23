import sys
from heapq import heappush, heappop

class G:
	id2idx = {}
	idx2id = [0]
	idx = 1
	edge = [[]]

DISTANCE_MULTIPLE = 100

g = G()

def get_idx(id):
	i = g.id2idx.get(id, 0)
	if i == 0:
		i = g.idx
		g.id2idx[id] = i
		g.idx2id.append(id)
		g.edge.append([])
		g.idx = g.idx + 1
	return i

def add_edge(start, end, distance):
	s = get_idx(start)
	e = get_idx(end)
	g.edge[s].append((e, distance))

def load():
	sys.stdin.readline()
	for line in sys.stdin:
		line = line.strip()
		data = line.split(",")
		# println(data)
		s = int(data[2])
		e = int(data[3])
		#d = int(float(data[5]) * 100)         # test failed
		d = int(float(data[5]) * 1000) // 10   # test passed
		if is_debug:
			print(f"line: {line} s: {s} e: {e} D: {d}")
		add_edge(s, e, d)

MAX_INT32 = 2147483647 # 2^31-1 - this is not the max value of python Number but good for this benchmark

def dijkstra(start, end):
	s = get_idx(start)
	e = get_idx(end)

	max_int = MAX_INT32
	size = g.idx
	d = [max_int] * size
	prev = [0] * size

	queue = []
	heappush(queue, (0, s))

	visited = 0
	while queue:
		distance, here = heappop(queue)
		if distance > d[here]:
			continue
		visited += 1
		if is_debug:
			print(f"visiting: {here} distance: {distance}")
		for to, weight in g.edge[here]:
			w = distance + weight
			if w < d[to]:
				prev[to] = here
				d[to] = w
				heappush(queue, (w, to))
	print("visited:", visited)

	n = e
	result = [g.idx2id[n]]

	while d[n] != max_int and n != s and n:
		n = prev[n]
		result.append(g.idx2id[n])

	return d[e] // DISTANCE_MULTIPLE, result  # 'x // y' is equvarent to 'int(x/y)'

def main():
	count = int(sys.argv[1])
	global is_debug
	is_debug = len(sys.argv) > 2 and sys.argv[2] == "debug"

	load()
	print("loaded nodes:", g.idx)

	route = []
	for i in range(1,count+1):
		s = g.idx2id[i*1000]
		distance, route = dijkstra(s, g.idx2id[1])
		print("distance:", distance)

	result = " ".join( str(id) for id in route )
	print("route: " + result + " ")  # original code has a small bug which prints tailing space
	#print("route:", result)

main()

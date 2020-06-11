import sys
from heapq import heappush, heappop

class G:
	id2idx = {}
	idx2id = [0]
	idx = 1
	edge = [[]]

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
	for line in sys.stdin.readlines():
		data = line.split(",")
		# println(data)
		s = int(data[2])
		e = int(data[3])
		d = float(data[5])
		add_edge(s, e, int(d*1000))

def dijkstra(start, end):
	s = get_idx(start)
	e = get_idx(end)

	size = g.idx
	d = [0] * size
	prev = [0] * size

	queue = []
	heappush(queue, (0, s))

	visited = 0
	while len(queue) > 0:
		distance, here = heappop(queue)
		visited = visited + 1
		# print("visiting:", here, " distance:", distance)
		for to, weight in g.edge[here]:
			w = distance + weight
			if d[to] == 0 or w < d[to]:
				prev[to] = here
				d[to] = w
				heappush(queue, (w, to))
	print("visited:", visited)

	n = e
	result = [g.idx2id[n]]

	while d[n] != 0 and n != s and n != 0:
		n = prev[n]
		result.append(g.idx2id[n])

	return int(d[e] / 1000), result

def main():
	count = int(sys.argv[1])

	load()
	print("loaded nodes:", g.idx)

	route = []
	for i in range(0,count):
		s = g.idx2id[(i+1)*1000+1]
		distance, route = dijkstra(s, g.idx2id[1])
		print("distance:", distance)

	result = "route: "
	for id in route:
		result = result + str(id) + " "
	print(result)

main()

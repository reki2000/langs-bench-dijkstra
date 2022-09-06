from heapq import heappush, heappop
from cython.view cimport array as cvarray

cdef class G:
    cdef dict id2idx
    cdef list idx2id
    cdef int idx
    cdef list edge

    def __init__(self):
        self.id2idx = {}
        self.idx2id = [0]
        self.idx = 1
        self.edge = [[]]

cdef int get_idx(G g, int id):
    cdef int i
    i = g.id2idx.get(id, 0)
    if i == 0:
        i = g.idx
        g.id2idx[id] = i
        g.idx2id.append(id)
        g.edge.append([])
        g.idx = g.idx + 1
    return i

cdef add_edge(G g, int start, int end, distance):
    cdef int s
    cdef int e
    s = get_idx(g, start)
    e = get_idx(g, end)
    g.edge[s].append((e, distance))

cdef int stof100(str s):
    cdef int result
    cdef int place
    cdef int is_decimal_place
    result = 0
    place = 2
    is_decimal_place = False
    for ch in s:
        if ch == '.':
            is_decimal_place = True
            continue
        result *= 10
        result += ord(ch) - ord('0')
        if is_decimal_place:
            place -= 1
            if place == 0:
                break
    while place > 0:
        result *= 10
        place -= 1
    return result

cdef tuple dijkstra(G g, int DISTANCE_MULTIPLE, is_debug, int start, int end):
    cdef int s
    cdef int e
    s = get_idx(g, start)
    e = get_idx(g, end)

    cdef int MAX_INT32
    MAX_INT32 = 2147483647 # 2^31-1 - this is not the max value of python Number but good for this benchmark

    cdef int size
    cdef int[:] d
    cdef int[:] prev
    size = g.idx
    d = cvarray(shape=(size,), itemsize=sizeof(int), format="i")
    prev = cvarray(shape=(size,), itemsize=sizeof(int), format="i")
    for i in range(size):
        d[i] = MAX_INT32
        prev[i] = 0

    cdef list queue
    queue = []
    heappush(queue, (0, s))

    cdef int visited
    visited = 0

    cdef int distance
    cdef int here
    cdef int to
    cdef int weight
    cdef int w
    while len(queue) > 0:
        distance, here = heappop(queue)
        if distance > d[here]:
            continue
        visited = visited + 1
        if is_debug:
            print(f"visiting: {here} distance: {distance}")
        for to, weight in g.edge[here]:
            w = distance + weight
            if w < d[to]:
                prev[to] = here
                d[to] = w
                heappush(queue, (w, to))
    print(f"visited: {visited}")

    cdef int n
    cdef list result
    n = e
    result = [g.idx2id[n]]

    while d[n] != MAX_INT32 and n != s and n != 0:
        n = prev[n]
        result.append(g.idx2id[n])

    return int(d[e] / DISTANCE_MULTIPLE), result

cpdef load(g, is_debug):
    import sys
    sys.stdin.readline()
    for line in sys.stdin.readlines():
        line = line.strip()
        data = line.split(",")
        # println(data)
        s = int(data[2])
        e = int(data[3])
        d = stof100(data[5])
        if is_debug:
            print(f"line: {line} s: {s} e: {e} D: {d}")
        add_edge(g, s, e, d)

cpdef main(count, is_debug):
    g = G()
    DISTANCE_MULTIPLE = 100
    load(g, is_debug)
    print(f"loaded nodes: {g.idx}")

    route = []
    for i in range(1,count+1):
        s = g.idx2id[i*1000]
        distance, route = dijkstra(g, DISTANCE_MULTIPLE, is_debug, s, g.idx2id[1])
        print(f"distance: {distance}")

    result = "route: "
    for id in route:
        result = result + str(id) + " "
    print(result)

package main

import (
	"encoding/csv"
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
)

type Edge struct {
	first  NodeIndex
	second Distance
}

type NodeId int
type NodeIndex int
type Distance int32

var is_debug = false

type G struct {
	id2idx map[NodeId]NodeIndex
	idx2id []NodeId
	idx    NodeIndex
	edge   [][]Edge
}

const DISTANCE_MULTIPLE = 100

func NewGraph() G {
	g := G{map[NodeId]NodeIndex{}, []NodeId{0}, 1, [][]Edge{nil}}
	return g
}

var g G = NewGraph()

func get_idx(id NodeId) NodeIndex {
	i, ok := g.id2idx[id]
	if !ok {
		i = g.idx
		g.id2idx[id] = i
		g.idx2id = append(g.idx2id, id) // equals to `g.idx2id[i] = id`
		g.edge = append(g.edge, []Edge{})
		g.idx++
	}
	return i
}

func add_edge(start NodeId, end NodeId, distance Distance) {
	s := get_idx(start)
	e := get_idx(end)
	g.edge[s] = append(g.edge[s], Edge{e, distance})
}

func stof100(s string) int {
	result := 0
	place := 2
	is_decimal_part := false
	for _, ch := range s {
		if ch == '.' {
			is_decimal_part = true
			continue
		}
		result *= 10
		result += int(ch) - int('0')
		if is_decimal_part {
			place--
			if place == 0 {
				break
			}
		}
	}
	for place > 0 {
		result *= 10
		place--
	}
	return result
}

func load() {
	reader := csv.NewReader(os.Stdin)
	line, err := reader.Read() // skip header

	for {
		line, err = reader.Read()
		if err != nil {
			break
		}
		s, _ := strconv.Atoi(line[2])
		e, _ := strconv.Atoi(line[3])
		d := stof100(line[5])
		add_edge(NodeId(s), NodeId(e), Distance(d))
		if is_debug {
			fmt.Println("line:", strings.Join(line, ","), "s:", s, "e:", e, "D:", Distance(d))
		}
	}
}

type Visit struct {
	first  Distance
	second NodeIndex
}

func dijkstra(start NodeId, end NodeId) (Distance, []NodeId) {
	s := get_idx(start)
	e := get_idx(end)

	size := g.idx

	d := make([]Distance, size)
	for i, _ := range d {
		d[i] = math.MaxInt32
	}
	prev := make([]NodeIndex, size)

	queue := NewPriorityQueue()
	queue.Push(Visit{0, s})

	visited := 0
	for !queue.Empty() {
		a := queue.Pop()
		distance := a.first
		here := a.second
		if distance > d[here] {
			continue
		}
		if is_debug {
			fmt.Println("visiting:", here, "distance:", distance)
		}
		visited++
		for _, e := range g.edge[here] {
			to := e.first
			w := distance + e.second
			if w < d[to] {
				prev[to] = here
				d[to] = w
				queue.Push(Visit{w, to})
			}
		}
	}
	fmt.Println("visited:", visited)

	n := e
	result := []NodeId{g.idx2id[n]}

	for d[n] != math.MaxInt32 && n != s && n != 0 {
		n = prev[n]
		result = append(result, g.idx2id[n])
	}

	return d[e] / DISTANCE_MULTIPLE, result
}

func main() {
	count, _ := strconv.Atoi(os.Args[1])
	is_debug = len(os.Args) > 2 && os.Args[2] == "debug"

	load()
	fmt.Println("loaded nodes:", g.idx)

	var distance Distance
	var route []NodeId
	for i := 0; i < count; i++ {
		s := g.idx2id[(i+1)*1000]
		distance, route = dijkstra(s, g.idx2id[1])
		fmt.Println("distance:", distance)
	}

	fmt.Print("route: ")
	for _, id := range route {
		fmt.Print(id, " ")
	}
	fmt.Println()
}

package main

import (
	"encoding/csv"
	"fmt"
	"os"
	"strconv"
)

type Edge struct {
	first  NodeIndex
	second Distance
}

type NodeId int
type NodeIndex int
type Distance int

type G struct {
	id2idx map[NodeId]NodeIndex
	idx2id []NodeId
	idx    NodeIndex
	edge   [][]Edge
}

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
		d, _ := strconv.ParseFloat(line[5], 32)
		add_edge(NodeId(s), NodeId(e), Distance(d*1000))
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
	prev := make([]NodeIndex, size)

	queue := NewPriorityQueue()
	queue.Push(Visit{0, s})

	visited := 0
	for !queue.Empty() {
		a := queue.Pop()
		visited++
		distance := a.first
		here := a.second
		//fmt.Println("visiting:", here, " distance:", distance)
		for _, e := range g.edge[here] {
			to := e.first
			w := distance + e.second
			if d[to] == 0 || w < d[to] {
				prev[to] = here
				d[to] = w
				queue.Push(Visit{w, to})
			}
		}
	}
	fmt.Println("visited:", visited)

	n := e
	result := []NodeId{g.idx2id[n]}

	for d[n] != 0 && n != s && n != 0 {
		n = prev[n]
		result = append(result, g.idx2id[n])
	}

	return d[e] / 1000, result
}

func main() {
	count, _ := strconv.Atoi(os.Args[1])

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

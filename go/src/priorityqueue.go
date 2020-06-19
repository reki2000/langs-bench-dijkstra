package main

type PriorityQueue struct {
	tree []Visit
}

func NewPriorityQueue() *PriorityQueue {
	return &PriorityQueue{[]Visit{}}
}

func (q *PriorityQueue) isPrior(i, j int) bool {
	a, b := q.tree[i].first, q.tree[j].first
	return a < b || (a == b && q.tree[i].second < q.tree[j].second)
}

func (q *PriorityQueue) Empty() bool {
	return len(q.tree) == 0
}

func (q *PriorityQueue) swap(i, j int) {
	q.tree[i], q.tree[j] = q.tree[j], q.tree[i]
}

func (q *PriorityQueue) Push(v Visit) {
	index := len(q.tree)
	q.tree = append(q.tree, v)
	for index > 0 {
		parentIndex := ((index + 1) >> 1) - 1
		if q.isPrior(index, parentIndex) {
			q.swap(index, parentIndex)
			index = parentIndex
		} else {
			break
		}
	}
}

func (q *PriorityQueue) Pop() Visit {
	result := q.tree[0]
	size := len(q.tree)
	q.tree[0] = q.tree[size-1]
	size--
	q.tree = q.tree[:size]
	for index := 0; ; {
		child := ((index + 1) << 1) - 1
		if child >= size {
			break
		}
		rIndex := child + 1
		if rIndex < size && q.isPrior(rIndex, child) {
			child = rIndex
		}
		if q.isPrior(index, child) {
			break
		}
		q.swap(index, child)
		index = child
	}
	return result
}

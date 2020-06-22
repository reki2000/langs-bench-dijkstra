import Base.pop!

const NodeId = UInt32
const NodeIndex = UInt32
const Distance = Int32

struct Visit
	first::Distance
	second::NodeIndex
end

struct PriorityQueue
	tree::Array{Visit, 1}
end


@inline function is_prior(q::PriorityQueue, i::UInt32, j::UInt32)::Bool 
	@inbounds local a = q.tree[i]
	@inbounds local b = q.tree[j]
	a.first < b.first || (a.first == b.first && a.second < b.second)
end

@inline function swap!(q::PriorityQueue, i::UInt32, j::UInt32) 
	@inbounds q.tree[i], q.tree[j] = q.tree[j], q.tree[i]
end


function empty(q::PriorityQueue)::Bool 
	return length(q.tree) == 0
end

function push!(q::PriorityQueue, v::Visit)
	index::UInt32 = length(q.tree) + 1
	push!(q.tree, v)
	while index > 1
		parentIndex::UInt32 = index >> 1
		if is_prior(q, index, parentIndex) 
			swap!(q, index, parentIndex)
			index = parentIndex
		else 
			break
		end
	end
end

function pop!(q::PriorityQueue)::Visit 
	result = q.tree[1]
	top = pop!(q.tree)
	size = length(q.tree)
	if size == 0
		return top
	end
	q.tree[1] = top

	index::UInt32 = 1
	while true
		child::UInt32 = (index  << 1)
		if child > size 
			break
		end
		rIndex::UInt32 = child + 1
		if rIndex <= size && is_prior(q, rIndex, child) 
			child = rIndex
		end
		if is_prior(q, index, child) 
			break
		end
		swap!(q, index, child)
		index = child
	end
	result
end

import Base.push!
#using Profile

include("priorityqueue.jl")

mutable struct Edge
	first::NodeIndex
	second::Distance
end

mutable struct G 
	id2idx::Dict{NodeId,NodeIndex}
	idx2id::Array{NodeId,1}
	idx::NodeIndex
	edge::Array{Array{Edge,1},1}
end

g = G(Dict{NodeId,NodeIndex}(), Array{NodeId,1}(), NodeIndex(1), Array{Array{Edge,1},1}())

function get_idx(id::NodeId)::NodeIndex 
  i = get(g.id2idx, id, 0)
  if i == 0
    i = g.idx
    g.id2idx[id] = i
    push!(g.idx2id, id)
    push!(g.edge, Array{Edge,1}())
    g.idx = g.idx + 1
  end
	i
end

function add_edge(start::NodeId, _end::NodeId, distance::Distance) 
	local s = get_idx(start)
	local e = get_idx(_end)
	push!(g.edge[s], Edge(e, distance))
end

function load() 
	i = 0
	for line in eachline(stdin)
		i = i + 1
		if i == 1
			continue
		end
		data = split(line, ",")
		# println(data)
		s = parse(Int32, data[3])
		e = parse(Int32, data[4])
		d = parse(Float32, data[6])
		add_edge(NodeId(s), NodeId(e), Distance(trunc(Distance, d*1000)))
	end
end

mutable struct Result
	first::Distance
	second::Array{NodeId}
end

function dijkstra(start::NodeId, _end::NodeId)::Result
	s = get_idx(start)
	e = get_idx(_end)

	size = g.idx
	d = zeros(Distance, size)
	prev = zeros(NodeIndex, size)

	queue = PriorityQueue([])
	push!(queue, Visit(0, s))

	visited = 0
	@inbounds while ! empty(queue) 
		a = pop!(queue)
		visited = visited + 1
		distance = a.first
		here = a.second
		# print("visiting:", here, " distance:", distance)
		for e in g.edge[here] 
			to = e.first
			w = distance + e.second
			if d[to] == 0 || w < d[to] 
				prev[to] = here
				d[to] = w
				push!(queue, Visit(w, to))
			end
		end
	end
	println("visited:", visited)

	n = e
	result = [g.idx2id[n]]

	while d[n] != 0 && n != s && n != 0 
		n = prev[n]
		push!(result, g.idx2id[n])
	end

	return Result(trunc(Distance, d[e] / 1000), result)
end

function main() 
	count = parse(Int, ARGS[1])

	load()
	println("loaded nodes:", g.idx)

	route = []
	for i in 0:count 
		s = g.idx2id[(i+1)*1000+1]
		result = dijkstra(s, g.idx2id[1])
		distance = result.first
		route = result.second
		println("distance:", distance)
	end

	print("route: ")
	for id in route 
		print(id, " ")
	end
	println()
end

main()

# @profile main()
# Profile.print()

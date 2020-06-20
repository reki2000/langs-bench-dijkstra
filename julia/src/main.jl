import Base.push!
#using Profile

include("priorityqueue.jl")

const DISTANCE_MULTIPLE = 100

struct Edge
    first::NodeIndex
    second::Distance
end

mutable struct G 
    id2idx::Dict{NodeId,NodeIndex}
    idx2id::Array{NodeId,1}
    idx::NodeIndex
    edge::Array{Array{Edge,1},1}
end

function get_idx!(g::G, id::NodeId)::NodeIndex 
    i = get(g.id2idx, id, 0)
    if i == 0
        i = g.idx
        g.id2idx[id] = i
        push!(g.idx2id, id)
        push!(g.edge, Array{Edge,1}())
        g.idx += 1
    end
    i
end

function add_edge!(g::G, start::NodeId, _end::NodeId, distance::Distance) 
    s = get_idx!(g, start)
    e = get_idx!(g, _end)
    push!(g.edge[s], Edge(e, distance))
end

function stof100(s::AbstractString)::Distance
    result::Distance = 0
    place = 0
    for ch in s
        if ch == '.'
            place = 1
            continue
        end
        result *= 10
        result += ch - '0'
        if place > 0
            place += 1
            if place >= 3
                break
            end
        end
    end
    while place < 3
        result *= 10
        place += 1
    end
    return result
end

function load!(g::G, is_debug::Bool = false)
    i = 0
    for line in eachline(stdin)
        i += 1
        if i == 1
            continue
        end
        data = split(line, ",")
        # println(data)
        s = parse(NodeId, data[3])
        e = parse(NodeId, data[4])
        d = stof100(data[6])
        is_debug && println("line: $(line) s: $(s) e: $(e) D: $(d)")
        add_edge!(g, s, e, d)
    end
end

struct Result
    first::Distance
    second::Array{NodeId, 1}
end

function dijkstra(g::G, start::NodeId, _end::NodeId, is_debug::Bool = false)::Result
    s = get_idx!(g, start)
    e = get_idx!(g, _end)

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
        is_debug && println("visiting: $(here) distance: $(distance)")
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
    println("visited: ", visited)

    n = e
    result = [g.idx2id[n]]

    while d[n] != 0 && n != s && n != 0 
        n = prev[n]
        push!(result, g.idx2id[n])
    end

    return Result(fld(d[e], DISTANCE_MULTIPLE), result)
end

function main() 
    count = parse(Int, ARGS[1])
    g = G(Dict{NodeId,NodeIndex}(), NodeId[], NodeIndex(1), Vector{Edge}[])
    is_debug = size(ARGS,1) > 1 && ARGS[2] == "debug"

    load!(g, is_debug)
    println("loaded nodes: ", g.idx)

    route = NodeId[]
    for i in 1:count 
        s = g.idx2id[i*1000]
        result = dijkstra(g, s, g.idx2id[1], is_debug)
        distance = result.first
        route = result.second
        println("distance: ", distance)
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

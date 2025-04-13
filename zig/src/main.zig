const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const AutoHashMap = std.AutoHashMap;
const PriorityQueue = std.PriorityQueue;
const math = std.math;
const io = std.io;
const fs = std.fs;

const NodeId = i32;
const NodeIndex = i32;
const Distance = i32;

const DISTANCE_MULTIPLE = 100;

const Edge = struct {
    index: NodeIndex,
    distance: Distance,
};

const Visit = struct {
    distance: Distance,
    index: NodeIndex,

    // For Zig 0.11.0, we need to use a custom comparison function
    pub fn compare(a: Visit, b: Visit) std.math.Order {
        if (a.distance < b.distance) {
            return .lt;
        } else if (a.distance > b.distance) {
            return .gt;
        } else {
            return std.math.order(a.index, b.index);
        }
    }
};

const Graph = struct {
    id2idx: AutoHashMap(NodeId, NodeIndex),
    idx2id: ArrayList(NodeId),
    idx: NodeIndex,
    edge: ArrayList(ArrayList(Edge)),
    allocator: Allocator,

    pub fn init(allocator: Allocator) !Graph {
        var id2idx = AutoHashMap(NodeId, NodeIndex).init(allocator);
        var idx2id = ArrayList(NodeId).init(allocator);
        try idx2id.append(0); // Start with 0 as placeholder
        
        var edge = ArrayList(ArrayList(Edge)).init(allocator);
        var empty_list = ArrayList(Edge).init(allocator);
        try edge.append(empty_list); // Start with empty list at index 0
        
        return Graph{
            .id2idx = id2idx,
            .idx2id = idx2id,
            .idx = 1,
            .edge = edge,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Graph) void {
        self.id2idx.deinit();
        self.idx2id.deinit();
        for (self.edge.items) |*edges| {
            edges.deinit();
        }
        self.edge.deinit();
    }

    pub fn getIdx(self: *Graph, id: NodeId) !NodeIndex {
        if (self.id2idx.get(id)) |i| {
            return i;
        } else {
            const i = self.idx;
            try self.id2idx.put(id, i);
            try self.idx2id.append(id);
            
            var edges = ArrayList(Edge).init(self.allocator);
            try self.edge.append(edges);
            
            self.idx += 1;
            return i;
        }
    }

    pub fn addEdge(self: *Graph, start: NodeId, end: NodeId, distance: Distance) !void {
        const s = try self.getIdx(start);
        const e = try self.getIdx(end);
        const s_usize = @as(usize, @intCast(s));
        try self.edge.items[s_usize].append(Edge{ .index = e, .distance = distance });
    }
};

fn stof100(s: []const u8) !Distance {
    var result: i32 = 0;
    var place: i32 = 2;
    var is_decimal_part = false;

    for (s) |ch| {
        if (ch == '.') {
            is_decimal_part = true;
            continue;
        }
        result *= 10;
        result += @as(i32, @intCast(ch - '0'));
        if (is_decimal_part) {
            place -= 1;
            if (place == 0) {
                break;
            }
        }
    }

    while (place > 0) {
        result *= 10;
        place -= 1;
    }

    return result;
}

fn load(graph: *Graph, reader: anytype, is_debug: bool) !void {
    var buf: [1024]u8 = undefined;
    
    // Skip header
    _ = try reader.readUntilDelimiterOrEof(&buf, '\n');
    
    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        if (line.len == 0) continue;
        
        var fields = std.mem.split(u8, line, ",");
        
        // Skip first two fields
        _ = fields.next();
        _ = fields.next();
        
        const start_str = fields.next() orelse continue;
        const end_str = fields.next() orelse continue;
        
        // Skip next field
        _ = fields.next();
        
        const distance_str = fields.next() orelse continue;
        
        const start = try std.fmt.parseInt(NodeId, start_str, 10);
        const end = try std.fmt.parseInt(NodeId, end_str, 10);
        const distance = try stof100(distance_str);
        
        if (is_debug) {
            std.debug.print("line: {s} s: {d} e: {d} D: {d}\n", .{line, start, end, distance});
        }
        
        try graph.addEdge(start, end, distance);
    }
}

const Result = struct {
    distance: Distance,
    route: ArrayList(NodeId),
};

fn dijkstra(graph: *Graph, start: NodeId, end: NodeId, allocator: Allocator, is_debug: bool) !Result {
    const s = try graph.getIdx(start);
    const e = try graph.getIdx(end);
    
    const size = graph.idx;
    const size_usize = @as(usize, @intCast(size));
    const max_int = math.maxInt(i32);
    
    var d = try allocator.alloc(Distance, size_usize);
    defer allocator.free(d);
    
    var prev = try allocator.alloc(NodeIndex, size_usize);
    defer allocator.free(prev);
    
    for (0..size_usize) |i| {
        d[i] = max_int;
        prev[i] = 0;
    }
    
    // Custom comparison for min-heap (reverse the order)
    const VisitContext = struct {
        pub fn compare(ctx: void, a: Visit, b: Visit) math.Order {
            _ = ctx;
            return Visit.compare(a, b);
        }
    };
    
    var queue = PriorityQueue(Visit, void, VisitContext.compare).init(allocator, {});
    defer queue.deinit();
    
    try queue.add(Visit{ .distance = 0, .index = s });
    d[@as(usize, @intCast(s))] = 0;
    
    var visited: i32 = 0;
    while (queue.count() > 0) {
        const current = queue.remove();
        const distance = current.distance;
        const here = current.index;
        const here_usize = @as(usize, @intCast(here));
        
        if (distance > d[here_usize]) continue;
        
        visited += 1;
        if (is_debug) {
            std.debug.print("visiting: {d} distance: {d}\n", .{here, distance});
        }
        
        for (graph.edge.items[here_usize].items) |edge| {
            const to = edge.index;
            const to_usize = @as(usize, @intCast(to));
            const w = distance + edge.distance;
            
            if (w < d[to_usize]) {
                prev[to_usize] = here;
                d[to_usize] = w;
                try queue.add(Visit{ .distance = w, .index = to });
            }
        }
    }
    
    std.debug.print("visited: {d}\n", .{visited});
    
    var result = ArrayList(NodeId).init(allocator);
    var n = e;
    const n_usize = @as(usize, @intCast(n));
    try result.append(graph.idx2id.items[n_usize]);
    
    while (d[@as(usize, @intCast(n))] != max_int and n != s and n != 0) {
        n = prev[@as(usize, @intCast(n))];
        try result.append(graph.idx2id.items[@as(usize, @intCast(n))]);
    }
    
    return Result{
        .distance = @divFloor(d[@as(usize, @intCast(e))], DISTANCE_MULTIPLE),
        .route = result,
    };
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    
    if (args.len < 2) {
        std.debug.print("Usage: {s} <count> [debug]\n", .{args[0]});
        return;
    }
    
    const count = try std.fmt.parseInt(i32, args[1], 10);
    const is_debug = args.len > 2 and std.mem.eql(u8, args[2], "debug");
    
    var graph = try Graph.init(allocator);
    defer graph.deinit();
    
    const stdin = std.io.getStdIn().reader();
    try load(&graph, stdin, is_debug);
    
    std.debug.print("loaded nodes: {d}\n", .{graph.idx});
    
    var result: Result = undefined;
    var i: i32 = 0;
    while (i < count) : (i += 1) {
        const idx = (i + 1) * 1000;
        const s = graph.idx2id.items[@as(usize, @intCast(idx))];
        result = try dijkstra(&graph, s, graph.idx2id.items[1], allocator, is_debug);
        std.debug.print("distance: {d}\n", .{result.distance});
    }
    
    std.debug.print("route: ", .{});
    for (result.route.items) |id| {
        std.debug.print("{d} ", .{id});
    }
    std.debug.print("\n", .{});
    
    result.route.deinit();
}

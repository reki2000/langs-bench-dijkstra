use std::cmp::Reverse;
use std::collections::BinaryHeap;
use std::env;
use std::io;
use std::io::prelude::*;

use rustc_hash::FxHashMap;

type NodeId = u32;
type NodeIndex = u32;
type Distance = u32;

const DISTANCE_MULTIPLE: u32 = 100;

struct Edge {
    first: NodeIndex,
    second: Distance,
}

struct G {
    id2idx: FxHashMap<NodeId, NodeIndex>,
    idx2id: Vec<NodeId>,
    idx: NodeIndex,
    edge: Vec<Vec<Edge>>,
}

fn get_idx(g: &mut G, id: NodeId) -> NodeIndex {
    if let Some(i) = g.id2idx.get(&id) {
        return *i;
    }
    let i = g.idx;
    g.id2idx.insert(id, i);
    g.idx2id.push(id);
    g.edge.push(vec![]);
    g.idx += 1;
    i
}

fn add_edge(g: &mut G, start: NodeId, end: NodeId, distance: Distance) {
    let s = get_idx(g, start);
    let e = get_idx(g, end);
    g.edge[(s as usize)].push(Edge {
        first: e,
        second: distance,
    });
}

fn stof100(s: &str) -> u32 {
    let mut result: u32 = 0;
    let mut place: u32 = 0;
    for ch in s.chars() {
        if ch == '.' {
            place = 1;
            continue;
        }
        result *= 10;
        result += (ch as u32) - ('0' as u32);
        if place > 0 {
            place += 1;
            if place >= 3 {
                break;
            }
        }
    }
    while place < 3 {
        result *= 10;
        place += 1;
    }
    result
}

fn load(g: &mut G) {
    for (index, line) in io::stdin().lock().lines().enumerate() {
        if index == 0 {
            continue;
        }
        if let Ok(l) = line {
            let fields: Vec<&str> = l.split(',').collect();
            let s: u32 = fields[2].parse().unwrap();
            let e: u32 = fields[3].parse().unwrap();
            let d: u32 = stof100(fields[5]);
            unsafe {
                if IS_DEBUG {
                    println!("line: {} s: {} e: {} D: {}", l, s, e, d);
                }
            }
            add_edge(g, s, e, d as Distance);
        }
    }
}

// Disable Clippy warning for a lint: more than 4 bindings with single-character names in a scope.
// https://rust-lang.github.io/rust-clippy/master/index.html#many_single_char_names
#[allow(clippy::many_single_char_names)]
fn dijkstra(g: &G, start: NodeId, end: NodeId) -> (Distance, Vec<NodeId>) {
    let s = *g.id2idx.get(&start).unwrap();
    let e = *g.id2idx.get(&end).unwrap();

    let size = g.idx as usize;
    let mut d = vec![0 as Distance; size];
    let mut prev = vec![0 as NodeIndex; size];

    //let mut vv = vec![0; size];

    let mut queue = BinaryHeap::new();
    queue.push((Reverse(0), Reverse(s)));

    let mut visited = 0;
    while let Some(v) = queue.pop() {
        let (Reverse(distance), Reverse(here)) = (v.0, v.1);
        unsafe {
            if IS_DEBUG {
                println!("visiting: {} distance: {}", here, distance);
            }
        }
        visited += 1;
        //println!("visiting: {} distance: {} qlen:{} visited:{} avg:{}, max:{}", here, distance, queue.len(), visited, sum/n, max);
        // std::thread::sleep(std::time::Duration::from_millis(1000));
        for edge in &g.edge[here as usize] {
            let (to, weight) = (edge.first as usize, edge.second);
            let w = distance + weight;
            // println!("visiting:{} distance:{} to:{}, d[to]:{}, w:{}, qlen:{} visited:{}", here, distance, to, d[to], w, queue.len(), visited);
            if d[to] == 0 || w < d[to] {
                prev[to] = here;
                d[to] = w;
                // vv[to] = vv[to] + 1;
                queue.push((Reverse(w), Reverse(to as NodeIndex)));
            }
        }
    }
    println!("visited: {}", visited);

    let mut n = e;
    let mut result = vec![g.idx2id[n as usize]];

    while d[n as usize] != 0 && n != s && n != 0 {
        n = prev[n as usize];
        result.push(g.idx2id[n as usize]);
    }

    ((d[e as usize] / DISTANCE_MULTIPLE) as Distance, result)
}

static mut IS_DEBUG: bool = false;

fn main() {
    let args: Vec<String> = env::args().collect();
    let count: i32 = args[1].parse().unwrap();
    unsafe {
        IS_DEBUG = args.len() > 2 && args[2] == "debug";
    }

    let mut g = G {
        id2idx: FxHashMap::default(),
        idx2id: vec![0],
        idx: 1,
        edge: vec![vec![]],
    };

    load(&mut g);
    println!("loaded nodes: {}", g.idx);

    let mut distance: Distance;
    let mut route = vec![0 as NodeId; 0];
    for i in 0..count {
        let s = g.idx2id[((i + 1) * 1000) as usize];
        let result = dijkstra(&g, s, g.idx2id[1]);
        distance = result.0;
        route = result.1;
        println!("distance: {}", distance);
    }

    let mut result = String::from("route: ");
    for id in route {
        result = result + &id.to_string() + " ";
    }
    println!("{}", result);
}

'use strict'

import fs from 'node:fs';
import {parse} from 'csv-parse/sync';
import pq from "./priorityqueue.js";

var is_debug = false;
const DISTANCE_MULTIPLE = 100;

let g = {
	id2idx: {},
	idx2id: [],
	idx:    1,
	edge:   []
}

function get_idx(id) {
	let i;
	if (id in g.id2idx) {
		i = g.id2idx[id];
	} else {
		i = g.idx;
		g.id2idx[id] = i;
		g.idx2id[i] = id;
		g.edge[i] = [];
		g.idx++;
	}
	return i;
}

function add_edge(start, end, distance) {
	const s = get_idx(start);
	const e = get_idx(end);
	g.edge[s].push([e, distance]);
}

function stof100(s) {
	let result = 0;
	let place = 2;
	let is_decimal_place = false;
	for (const ch of s) {
	  if (ch == '.') {
		is_decimal_place = true;
		continue;
	  }
	  result *= 10;
	  result += ch - '0';
	  if (is_decimal_place) {
		place--;
		if (place == 0) {
			break;
		}
	  }
	}
	while (place > 0) {
	  result *= 10;
	  place--;
	}
	return result;
}

function load() {
	let data = fs.readFileSync('/dev/stdin', 'utf8');
	let res = parse(data);
	
	let i = 0;
	for (let line of res) {
		i++;
		if (i === 1) continue;
		const s = parseInt(line[2]);
		const e = parseInt(line[3]);
		const d = stof100(line[5]);
		if (is_debug) console.log("line:",line.join(","), "s:",s,"e:",e,"D:",d);
		add_edge(s, e, d);
	}
}

function dijkstra(start , end ) {
	const s = get_idx(start);
	const e = get_idx(end);

	const MAX_INT32 = 2147483647; // 2^31-1 - this is not the max value of javascript Number but good for this benchmark
	const size = g.idx;
	const d = new Array(size+1).fill(MAX_INT32); 
	const prev = new Array(size+1).fill(0);

	const queue = new pq();
	queue.Push([0, s]);

	let visited = 0;
	while (!queue.Empty()) {
		const [distance, here] = queue.Pop();
		if (distance > d[here]) continue;
		visited++;;
		if (is_debug) console.log("visiting:", here, "distance:", distance);
		for (let e of g.edge[here]) {
			const to = e[0];
			const w = distance + e[1];
			if (w < d[to]) {
				prev[to] = here;
				d[to] = w;
				queue.Push([w, to]);
			}
		}
	}
	console.log("visited:" , visited);

	let n = e;
	let result = [n];

	while (d[n] !== MAX_INT32 && n !== s && n !== 0) {
		n = prev[n];
		result.push(g.idx2id[n]);
	}

	return [(d[e] / DISTANCE_MULTIPLE)|0, result];
}

function main() {
	const count = parseInt(process.argv[2]);
	is_debug = process.argv.length > 3 && process.argv[3] == 'debug';

	load();
	console.log("loaded nodes:", g.idx);

	let distance = 0, route = [];
	for (let i = 1; i <= count; i++) {
		const s = g.idx2id[i*1000];
		[distance, route] = dijkstra(s, g.idx2id[1]);
		console.log("distance:", distance);
	}

	let line = "route: ";
	for (let id of route) {
		line += id + " ";
	}
	console.log(line);
}

main();

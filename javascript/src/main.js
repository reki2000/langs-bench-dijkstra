'use strict'

const fs = require('fs');
const csvSync = require('csv-parse/lib/sync'); // requiring sync module
const pq = require('./priorityqueue');

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

function load() {
	let data = fs.readFileSync('/dev/stdin', 'utf8');
	let res = csvSync(data);
;
	let i = 0;
	for (let line of res) {
		i++;
		if (i === 1) continue;
		const s = parseInt(line[2]);
		const e = parseInt(line[3]);
		const d = parseFloat(line[5]);
		add_edge(s, e, (d*100)|0 /* x|0 makes int from float */);
	}
}

function dijkstra(start , end ) {
	const s = get_idx(start);
	const e = get_idx(end);

	const size = g.idx;
	const d = new Array(size+1).fill(0);
	const prev = new Array(size+1).fill(0);

	const queue = new pq();
	queue.Push([0, s]);

	let visited = 0;
	while (!queue.Empty()) {
		const [distance, here] = queue.Pop();
		visited++;;
		//console.log("visiting:", here, " distance:", distance);
		for (let e of g.edge[here]) {
			const to = e[0];
			const w = distance + e[1];
			//console.log("visiting:", to, " distance:", w, d[to]);
			if (d[to] === 0 || w < d[to]) {
				prev[to] = here;
				d[to] = w;
				queue.Push([w, to]);
			}
		}
	}
	console.log("visited:" , visited);

	let n = e;
	let result = [n];

	while (d[n] !== 0 && n !== s && n !== 0) {
		n = prev[n];
		result.push(g.idx2id[n]);
	}

	return [(d[e] / 1000)|0, result];
}

function main() {
  const count = parseInt(process.argv[2]);

	load();
	console.log("loaded nodes:", g.idx);

	let distance = 0, route = [];
	for (let i = 0; i < count; i++) {
		const s = g.idx2id[(i+1)*1000];
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

import 'pair.dart';
import 'priorityqueue.dart';
import 'dart:io';
import 'dart:convert';

typedef NodeId = int;
typedef NodeIndex = int;
typedef Distance = int;

typedef Edge = Pair<NodeIndex, Distance>;

const DISTANCE_MULTIPLE = 100;

var isDebug = false;

class G {
  final id2idx = <NodeId, NodeIndex>{};
  final idx2id = List<NodeId>.of([0]);
  var idx = 1;
  final edge = List<List<Edge>>.of([
    [Edge(0, 0)]
  ]);
}

final g = G();

NodeIndex get_idx(NodeId id) {
  final index = g.id2idx[id];
  if (index != null) {
    return index;
  }
  final i = g.idx++;
  g.id2idx[id] = i;
  g.idx2id.add(id);
  g.edge.add(List<Edge>.of([]));
  return i;
}

void add_edge(NodeId start, end, Distance distance) {
  final s = get_idx(start);
  final e = get_idx(end);
  g.edge[s].add(Edge(e, distance));
}

final charDot = '.'.codeUnitAt(0);
final charZero = '0'.codeUnitAt(0);

int stof100(String s) {
  var result = 0;
  var place = 2;
  var is_decimal_place = false;
  for (final ch in s.codeUnits) {
    if (ch == charDot) {
      is_decimal_place = true;
      continue;
    }
    result *= 10;
    result += ch - charZero;
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

Future<void> load() async {
  stdin.readLineSync();

  final lines = stdin.transform(utf8.decoder).transform(LineSplitter());

  // var lineNo = 1;
  // final sw = Stopwatch()..start();

  await for (final line in lines) {
    final fields = line.split(',');
    final s = int.parse(fields[2]);
    final e = int.parse(fields[3]);
    final d = stof100(fields[5]);

    // if (lineNo % 100000 == 0 && isDebug) {
    //   final elapsed = sw.elapsedMilliseconds;
    //   sw.reset();

    //   print("elapsed: $elapsed line: $line s: $s e: $e D: $d");
    // }
    // lineNo++;

    add_edge(s, e, d);
  }
}

typedef Visit = Pair<Distance, NodeIndex>;
typedef Result = Pair<Distance, List<NodeId>>;

const intMaxValue = 9223372036854775807;

Result dijkstra(NodeId start, end) {
  final s = get_idx(start);
  final e = get_idx(end);

  final size = g.idx;
  final d = List<Distance>.generate(size, (_) => intMaxValue);
  final prev = List<NodeIndex>.generate(size, (_) => 0);

  final queue = PriorityQueue();
  queue.push(Visit(0, s));

  var visited = 0;

  while (!queue.empty()) {
    final pop = queue.pop();
    final distance = pop.first;
    final here = pop.second;
    if (distance > d[here]) {
      continue;
    }
    visited++;
    if (isDebug) print("visiting: $here distance: $distance");

    for (final pop in g.edge[here]) {
      final to = pop.first;
      final weight = pop.second;
      final w = distance + weight;
      if (w < d[to]) {
        prev[to] = here;
        d[to] = w;
        queue.push(Visit(w, to));
      }
    }
  }
  print("visited: $visited");

  var n = e;
  final result = <int>[];
  result.add(g.idx2id[n]);

  while (d[n] != intMaxValue && n != s && n != 0) {
    n = prev[n];
    result.add(g.idx2id[n]);
  }

  return Result(d[e] ~/ DISTANCE_MULTIPLE, result);
}

void main(List<String> args) async {
  final count = int.parse(args[0]);
  isDebug = args.length > 1 && args[1] == "debug";

  await load();
  print("loaded nodes: ${g.idx}");

  Distance distance;
  var route = List<NodeId>.of([]);
  for (var i = 1; i < count; i++) {
    final s = g.idx2id[i * 1000];
    final result = dijkstra(s, g.idx2id[1]);
    distance = result.first;
    route = result.second;
    print("distance: $distance");
  }

  print("route: ${route.join(" ")} ");
}

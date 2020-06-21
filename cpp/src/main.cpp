#include <bits/stdc++.h>
using namespace std;

using NodeId = int;
using NodeIndex = int;
using Distance = int;
using Edge = pair<NodeIndex, Distance>;

const int DISTANCE_MULTIPLE = 100;

bool is_debug = false;

struct G {
  map<NodeId,NodeIndex> id2idx;
  vector<NodeId> idx2id = {0};
  NodeIndex idx = 1;
  vector<vector<Edge>> edge = {vector<Edge>()};
} g;

NodeIndex get_idx(NodeId id) {
  NodeIndex i = g.id2idx[id];
  if (i == 0) {
    i = g.idx++;
    g.id2idx[id] = i;
    g.idx2id.push_back(id);
    g.edge.push_back(vector<Edge>());
  }
  return i;
}

void add_edge(NodeId start, NodeId end, Distance distance) {
  NodeIndex s = get_idx(start);
  NodeIndex e = get_idx(end);
  g.edge[s].push_back({e, distance});
}

// 123.4567 --> 12345
int stof100(const char *s) {
  int result = 0;
  int place = 0;
  for (;*s != '\0'; s++) {
    if (*s == '.') {
      place = 1;
      continue;
    }
    result *= 10;
    result += *s - '0';
    if (place > 0) {
      place++;
      if (place >= 3) {
          break;
      }
    }
  }
  while (place < 3) {
    result *= 10;
    place++;
  }
  return result;
}

void load() {
  string line;
  cin >> line; // skip header

  while (true) {
    cin >> line;
    if (cin.eof()) {
      break;
    }
    int s = 0, e = 0;
    float d = 0;
    for (int idx=0, pos=0, prev_pos=0; pos <= line.length(); pos++) {
      if (line[pos] == ',' || pos == line.length()) {
        auto field = line.substr(prev_pos, pos-prev_pos);
        switch (idx) {
          case 2: s = stoi(field); break;
          case 3: e = stoi(field); break;
          case 5: d = stof100(field.c_str()); break;
        }
        prev_pos = pos+1;
        idx++;
      }
    }
    if (is_debug) cout << "line: " << line << " s: " << s << " e: " << e << " D: " << d << endl;
    // cerr << "line:" << line << "s:" << s << " e:" << e << " d:" << d << endl;
    // std::this_thread::sleep_for(std::chrono::seconds(1));
    add_edge(s, e, (int)d);
  }
}

using Visit = pair<Distance, NodeIndex>;

pair<Distance, vector<NodeId>> dijkstra(NodeId start, NodeId end) {
  NodeIndex s = get_idx(start);
  NodeIndex e = get_idx(end);

  int size = g.idx;
  std::vector<Distance> d(size);
  std::vector<NodeIndex> prev(size);

  priority_queue<Visit, vector<Visit>, greater<Visit>> queue;
  queue.push({0,s});

  int visited = 0;
  while (!queue.empty()) {
    auto a = queue.top();
    queue.pop();
    Distance distance = a.first;
    NodeIndex here = a.second;
    if (is_debug) cout << "visiting: " << here << " distance: " << distance << endl;
    visited++;
    for (Edge e : g.edge[here]) {
      NodeIndex to = e.first;
      Distance w = distance + e.second;
      if (d[to] == 0 || w < d[to]) {
        prev[to] = here;
        d[to] = w;
        queue.push({w, to});
      }
    }
  }

  cerr << "visited: " << visited << endl;

  vector<NodeId> result;
  NodeIndex n = e;
  result.push_back(g.idx2id[n]);

  while (d[n] != 0 && n != s && n != 0) {
    n = prev[n];
    result.push_back(g.idx2id[n]);
  }

  return {d[e] / DISTANCE_MULTIPLE, result};

}

int main(int argc, char **argv) {
  ios::sync_with_stdio(false);
  cin.tie(nullptr);

  int count = atoi(argv[1]);
  is_debug = argc > 2 && string(argv[2]) == "debug";

  load();
  cerr << "loaded nodes: " << g.idx << endl;

  pair<Distance, vector<NodeId>> result;
  for (int i=0; i<count; i++) {
    NodeId s = g.idx2id[(i+1) * 1000];
    result = dijkstra(s, g.idx2id[1]);
    cout << "distance: " << result.first << endl;
  }

  cout << "route: ";
  for (NodeId id: result.second) {
    cout << id << " ";
  }
  cout << endl;
}

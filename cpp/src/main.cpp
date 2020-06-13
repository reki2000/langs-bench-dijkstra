#include <bits/stdc++.h>
using namespace std;

using NodeId = int;
using NodeIndex = int;
using Distance = int;
using Edge = pair<NodeIndex, Distance>;

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
    for (int idx=0, pos=0, prev_pos=0; pos < line.length(); pos++) {
      if (line[pos] == ',' || pos == line.length() - 1) {
        auto field = line.substr(prev_pos, pos-prev_pos);
        switch (idx) {
          case 2: s = stoi(field); break;
          case 3: e = stoi(field); break;
          case 5: d = stof(field); break;
        }
        prev_pos = pos+1;
        idx++;
      }
    }
    // cerr << "line:" << line << "s:" << s << " e:" << e << " d:" << d << endl;
    // std::this_thread::sleep_for(std::chrono::seconds(1));
    add_edge(s, e, (int)(d * 1000));
  }
}

using Visit = pair<Distance, NodeIndex>;

pair<Distance, vector<NodeId>> dijkstra(NodeId start, NodeId end) {
  NodeIndex s = get_idx(start);
  NodeIndex e = get_idx(end);

  int size = g.idx;
  Distance d[size] = {};
  NodeIndex prev[size] = {};

  priority_queue<Visit, vector<Visit>, greater<Visit>> queue;
  queue.push({0,s});

  int visited = 0;
  while (!queue.empty()) {
    auto a = queue.top();
    queue.pop();
    Distance distance = a.first;
    NodeIndex here = a.second;
    // if (distance <= 0 && here != s) {
    //   cerr << "assert" << endl;
    //   exit(1);
    // }
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

  return {d[e] / 1000, result};

}

int main(int argc, char **argv) {
  ios::sync_with_stdio(false);
  cin.tie(nullptr);

  load();
  cerr << "loaded nodes: " << g.idx << endl;

  int count = atoi(argv[1]);

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

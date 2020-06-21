#include<vector>
#include<unordered_map>
#include<string>
#include<iostream>
#include<queue>

using NodeId = int;
using NodeIndex = int;
using Distance = int;
using Edge = std::pair<NodeIndex, Distance>;

constexpr int DISTANCE_MULTIPLE = 100;

bool is_debug = false;

struct G {
  std::unordered_map<NodeId,NodeIndex> id2idx;
  std::vector<NodeId> idx2id = {0};
  NodeIndex idx = 1;
  std::vector<std::vector<Edge>> edge = {std::vector<Edge>()};
} g;

NodeIndex get_idx(NodeId id) {
  NodeIndex i = g.id2idx[id];
  if (i == 0) {
    i = g.idx++;
    g.id2idx[id] = i;
    g.idx2id.push_back(id);
    g.edge.push_back(std::vector<Edge>());
  }
  return i;
}

void add_edge(NodeId start, NodeId end, Distance distance) {
  const NodeIndex s = get_idx(start);
  const NodeIndex e = get_idx(end);
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
  std::string line;
  std::cin >> line; // skip header

  while (true) {
    std::cin >> line;
    if (std::cin.eof()) {
      break;
    }
    int s = 0, e = 0;
    float d = 0;
    for (int idx=0, pos=0, prev_pos=0; static_cast<std::string::size_type>(pos) <= line.length(); pos++) {
      if (line[pos] == ',' || static_cast<std::string::size_type>(pos) == line.length()) {
        const auto field = line.substr(prev_pos, pos-prev_pos);
        switch (idx) {
          case 2: s = stoi(field); break;
          case 3: e = stoi(field); break;
          case 5: d = stof100(field.c_str()); break;
        }
        prev_pos = pos+1;
        idx++;
      }
    }
    if (is_debug) std::cout << "line: " << line << " s: " << s << " e: " << e << " D: " << d << std::endl;
    // cerr << "line:" << line << "s:" << s << " e:" << e << " d:" << d << endl;
    // std::this_thread::sleep_for(std::chrono::seconds(1));
    add_edge(s, e, (int)d);
  }
}

using Visit = std::pair<Distance, NodeIndex>;

std::pair<Distance, std::vector<NodeId>> dijkstra(NodeId start, NodeId end) {
  const NodeIndex s = get_idx(start);
  const NodeIndex e = get_idx(end);

  const int size = g.idx;
  std::vector<Distance> d(size);
  std::vector<NodeIndex> prev(size);

  std::priority_queue<Visit, std::vector<Visit>, std::greater<Visit>> queue;
  queue.push({0,s});

  int visited = 0;
  while (!queue.empty()) {
    const auto a = queue.top();
    queue.pop();
    const Distance distance = a.first;
    const NodeIndex here = a.second;
    if (is_debug) std::cout << "visiting: " << here << " distance: " << distance << std::endl;
    visited++;
    for (const Edge& e : g.edge[here]) {
      const NodeIndex to = e.first;
      const Distance w = distance + e.second;
      if (d[to] == 0 || w < d[to]) {
        prev[to] = here;
        d[to] = w;
        queue.push({w, to});
      }
    }
  }

  std::cerr << "visited: " << visited << std::endl;

  std::vector<NodeId> result;
  NodeIndex n = e;
  result.push_back(g.idx2id[n]);

  while (d[n] != 0 && n != s && n != 0) {
    n = prev[n];
    result.push_back(g.idx2id[n]);
  }

  return {d[e] / DISTANCE_MULTIPLE, result};

}

int main(int argc, char **argv) {
  std::ios::sync_with_stdio(false);
  std::cin.tie(nullptr);

  const int count = atoi(argv[1]);
  is_debug = argc > 2 && std::string(argv[2]) == "debug";

  load();
  std::cerr << "loaded nodes: " << g.idx << std::endl;

  std::pair<Distance, std::vector<NodeId>> result;
  for (int i=0; i<count; i++) {
    const NodeId s = g.idx2id[(i+1) * 1000];
    result = dijkstra(s, g.idx2id[1]);
    std::cout << "distance: " << result.first << std::endl;
  }

  std::cout << "route: ";
  for (const NodeId id: result.second) {
    std::cout << id << " ";
  }
  std::cout << std::endl;
}

#include<vector>
#include<unordered_map>
#include<string>
#include<string_view>
#include<queue>
#include<cstdint>
#include<limits>
#include"ankerl/unordered_dense.h"
#include"fast_io.h"

using NodeId = int;
using NodeIndex = int;
using Distance = std::int32_t;
using Edge = std::pair<NodeIndex, Distance>;

constexpr int DISTANCE_MULTIPLE = 100;

bool is_debug = false;

struct G {
  ankerl::unordered_dense::map<NodeId,NodeIndex> id2idx;
  std::vector<NodeId> idx2id = {0};
  NodeIndex idx = 1;
  std::vector<std::vector<Edge>> edge = {std::vector<Edge>()};
} g;

inline NodeIndex get_idx(NodeId id) {
  NodeIndex i = g.id2idx[id];
  if (i == 0) {
    i = g.idx++;
    g.id2idx[id] = i;
    g.idx2id.push_back(id);
    g.edge.push_back(std::vector<Edge>());
  }
  return i;
}

inline void add_edge(NodeId start, NodeId end, Distance distance) {
  const NodeIndex s = get_idx(start);
  const NodeIndex e = get_idx(end);
  g.edge[s].emplace_back(e, distance);
}

inline int stoi_unchecked(std::string_view s) {
  int result = 0;
  for(auto&& x : s) {
    result *= 10;
    result += x - '0';
  }
  return result;
}

// 123.4567 --> 12345
inline int stof100(std::string_view s) {
  int result = 0;
  int place = 2;
  auto it = s.cbegin();
  const auto end = s.cend();
  for(; it != end; ++it) {
    if (*it == '.') {
      ++it;
      break;
    }
    result *= 10;
    result += *it - '0';
  }
  for(; it != end && place-- > 0; ++it) {
    result *= 10;
    result += *it - '0';
  }
  while(place-- > 0) {
    result *= 10;
  }
  return result;
}

void load() {
  std::string line_buf;
  ::scan(fast_io::manipulators::line_get(line_buf)); // skip header

  while (::scan<true>(fast_io::manipulators::line_get(line_buf))) {
    std::string_view line(line_buf);
    while (!fast_io::char_category::is_c_graph(line.back())) line.remove_suffix(1); // strip
    const auto pos1 = line.find(',');
    const auto pos2 = line.find(',', pos1 + 1);
    const auto pos3 = line.find(',', pos2 + 1);
    const auto pos4 = line.find(',', pos3 + 1);
    const auto pos5 = line.find(',', pos4 + 1);
    const NodeId s = stoi_unchecked(line.substr(pos2+1, pos3-pos2-1));
    const NodeId e = stoi_unchecked(line.substr(pos3+1, pos4-pos3-1));
    const Distance d = stof100(line.substr(pos5+1));
    if (is_debug) ::println("line: ", line, " s: ", s, " e: ", e, " D: ", d);
    add_edge(s, e, d);
  }
}

using Visit = std::pair<Distance, NodeIndex>;

inline std::pair<Distance, std::vector<NodeId>> dijkstra(NodeId start, NodeId end) {
  const NodeIndex s = get_idx(start);
  const NodeIndex e = get_idx(end);

  const int size = g.idx;
  std::vector<Distance> d(size, std::numeric_limits<Distance>::max());
  std::vector<NodeIndex> prev(size);

  std::priority_queue<Visit, std::vector<Visit>, std::greater<>> queue;
  queue.push({0,s});

  int visited = 0;
  while (!queue.empty()) {
    const auto a = queue.top();
    queue.pop();
    const Distance distance = a.first;
    const NodeIndex here = a.second;
    if (distance > d[here]) continue;
    if (is_debug) ::println("visiting: ", here, " distance: ", distance);
    ++visited;

    for (const Edge& e : g.edge[here]) {
      const NodeIndex to = e.first;
      const Distance w = distance + e.second;
      if (w < d[to]) {
        prev[to] = here;
        d[to] = w;
        queue.push({w, to});
      }
    }
  }

  ::println("visited: ", visited);

  std::vector<NodeId> result;
  NodeIndex n = e;
  result.push_back(g.idx2id[n]);

  while (d[n] != std::numeric_limits<Distance>::max() && n != s && n != 0) {
    n = prev[n];
    result.push_back(g.idx2id[n]);
  }

  return {d[e] / DISTANCE_MULTIPLE, result};

}

int main(int argc, char **argv) {
  const int count = atoi(argv[1]);
  is_debug = argc > 2 && std::string_view(argv[2]) == "debug";

  load();
  ::println("loaded nodes: ", g.idx);

  std::pair<Distance, std::vector<NodeId>> result;
  for (int i=0; i<count; ++i) {
    const NodeId s = g.idx2id[(i+1) * 1000];
    result = dijkstra(s, g.idx2id[1]);
    ::println("distance: ", result.first);
  }

  ::print("route: ");
  for (const NodeId id: result.second) {
    ::print(id, fast_io::manipulators::chvw(' '));
  }
  ::print(fast_io::manipulators::chvw('\n'));
}

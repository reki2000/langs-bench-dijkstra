import 'pair.dart';

typedef P = Pair<int, int>;

class PriorityQueue {
  final tree = List<P>.of([]);

  void _swap(int i, j) {
    final tmp = tree[j];
    tree[j] = tree[i];
    tree[i] = tmp;
  }

  bool _isPrior(int i, j) {
    return tree[i].first < tree[j].first ||
        (tree[i].first == tree[j].first && tree[i].second < tree[j].second);
  }

  bool empty() => tree.isEmpty;

  void push(P v) {
    var index = tree.length;
    tree.add(v);
    while (index > 0) {
      final parentIndex = ((index + 1) >>> 1) - 1;
      if (_isPrior(index, parentIndex)) {
        _swap(index, parentIndex);
        index = parentIndex;
      } else {
        break;
      }
    }
  }

  P pop() {
    final result = tree[0];
    final last = tree.last;
    final size = tree.length - 1;
    tree.removeAt(size);
    if (size == 0) {
      return last;
    }
    tree[0] = last;
    var index = 0;
    while (true) {
      var child = ((index + 1) << 1) - 1;
      if (child >= size) {
        break;
      }
      final rIndex = child + 1;
      if (rIndex < size && _isPrior(rIndex, child)) {
        child = rIndex;
      }
      if (_isPrior(index, child)) {
        break;
      }
      _swap(index, child);
      index = child;
    }
    return result;
  }
}


DISTANCE_MULTIPLE = 100

class G
  attr_accessor :id2idx, :idx2id, :idx, :edge
  
  def initialize
    @id2idx = {}
    @idx2id = [0]
    @idx = 1
    @edge = [[]]
  end
end

$g = G.new
$is_debug = false

def get_idx(id)
  i = $g.id2idx[id] || 0
  if i == 0
    i = $g.idx
    $g.id2idx[id] = i
    $g.idx2id.push(id)
    $g.edge.push([])
    $g.idx += 1
  end
  i
end

def add_edge(start, end_node, distance)
  s = get_idx(start)
  e = get_idx(end_node)
  $g.edge[s].push([e, distance])
end

def stof100(s)
  result = 0
  place = 2
  is_decimal_part = false
  
  s.each_char do |ch|
    if ch == '.'
      is_decimal_part = true
      next
    end
    result *= 10
    result += ch.ord - '0'.ord
    if is_decimal_part
      place -= 1
      break if place == 0
    end
  end
  
  while place > 0
    result *= 10
    place -= 1
  end
  
  result
end

def load
  STDIN.gets
  
  STDIN.each_line do |line|
    line = line.strip
    next if line.empty?
    
    fields = line.split(',')
    s = fields[2].to_i
    e = fields[3].to_i
    d = stof100(fields[5])
    
    puts "line: #{line} s: #{s} e: #{e} D: #{d}" if $is_debug
    
    add_edge(s, e, d)
  end
end

class PriorityQueue
  def initialize
    @heap = []
  end
  
  def empty?
    @heap.empty?
  end
  
  def push(item)
    @heap.push(item)
    heapify_up(@heap.size - 1)
  end
  
  def pop
    return nil if @heap.empty?
    
    min = @heap[0]
    last = @heap.pop
    
    unless @heap.empty?
      @heap[0] = last
      heapify_down(0)
    end
    
    min
  end
  
  private
  
  def heapify_up(index)
    parent = (index - 1) / 2
    
    if index > 0 && compare(@heap[index], @heap[parent])
      @heap[index], @heap[parent] = @heap[parent], @heap[index]
      heapify_up(parent)
    end
  end
  
  def heapify_down(index)
    smallest = index
    left = 2 * index + 1
    right = 2 * index + 2
    
    smallest = left if left < @heap.size && compare(@heap[left], @heap[smallest])
    smallest = right if right < @heap.size && compare(@heap[right], @heap[smallest])
    
    if smallest != index
      @heap[index], @heap[smallest] = @heap[smallest], @heap[index]
      heapify_down(smallest)
    end
  end
  
  def compare(a, b)
    a[0] < b[0] || (a[0] == b[0] && a[1] < b[1])
  end
end

def dijkstra(start, end_node)
  s = get_idx(start)
  e = get_idx(end_node)
  
  size = $g.idx
  max_int = (2**31 - 1) # MAX_INT32
  
  d = Array.new(size, max_int)
  prev = Array.new(size, 0)
  
  queue = PriorityQueue.new
  queue.push([0, s])
  
  visited = 0
  until queue.empty?
    distance, here = queue.pop
    next if distance > d[here]
    
    visited += 1
    puts "visiting: #{here} distance: #{distance}" if $is_debug
    
    $g.edge[here].each do |edge|
      to, weight = edge
      w = distance + weight
      
      if w < d[to]
        prev[to] = here
        d[to] = w
        queue.push([w, to])
      end
    end
  end
  
  puts "visited: #{visited}"
  
  n = e
  result = [$g.idx2id[n]]
  
  while d[n] != max_int && n != s && n != 0
    n = prev[n]
    result.push($g.idx2id[n])
  end
  
  return d[e] / DISTANCE_MULTIPLE, result
end

def main
  count = ARGV[0].to_i
  $is_debug = ARGV.size > 1 && ARGV[1] == "debug"
  
  load
  puts "loaded nodes: #{$g.idx}"
  
  distance = 0
  route = []
  
  (0...count).each do |i|
    s = $g.idx2id[(i+1) * 1000]
    distance, route = dijkstra(s, $g.idx2id[1])
    puts "distance: #{distance}"
  end
  
  print "route: "
  route.each { |id| print "#{id} " }
  puts
end

main

function fib(n)::Int64
  if n <= 2
    return 1
  end
  return fib(n-1) + fib(n-2)
end

n = parse(Int, ARGS[1])
count = parse(Int, ARGS[2])
for i=1:count
  println(fib(n))
end

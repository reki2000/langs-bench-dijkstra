import sys

def fib(n):
  if n<=2:
    return 1
  return fib(n-2) + fib(n-1)

n = int(sys.argv[1])
count = int(sys.argv[2])

for i in range(count):
  print(fib(n))
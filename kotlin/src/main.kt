package main

fun fib(n: Int): Long {
  if (n <= 2) {
    return 1
  }
  return fib(n - 2) + fib(n - 1)
}

fun main(args: Array<String>) {
  var n = Integer.parseInt(args[0]) 
  var count = Integer.parseInt(args[1]) 
  for (i in 1..count) {
    println(fib(n))
  }
}

use std::env;

fn fib(n: i32) -> i64 {
  if n <= 2 {
    return 1;
  }
  return fib(n-1) + fib(n-2);
}

fn main() {
  let args: Vec<String> = env::args().collect();

  let n: i32 = args[1].parse().unwrap();
  let count: i32 = args[2].parse().unwrap();

  for _i in 0..count {
    println!("{}", fib(n));
  }
}
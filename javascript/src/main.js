function fib(n) {
  if (n <= 2) {
    return 1;
  }
  return fib(n-1) + fib(n-2);
}

const n = parseInt(process.argv[2]);
const count = parseInt(process.argv[3]);
for (var i=0; i<count; i++) {
  console.log(fib(n));
}

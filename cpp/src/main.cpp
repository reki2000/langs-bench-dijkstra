#include <bits/stdc++.h>
using namespace std;

long long fib(long long n) {
  if (n <= 2) {
    return 1;
  }
  return fib(n-2) + fib(n-1);
}

int main(int argc, char **argv) {
  int n = atoi(argv[1]);
  int count = atoi(argv[2]);

  for (int i=0; i<count; i++) {
    cout << fib(n) << endl;
  }
}
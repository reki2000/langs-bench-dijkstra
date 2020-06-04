package main

import (
	"fmt"
	"os"
	"strconv"
)

func fib(n int) int64 {
	if n <= 2 {
		return 1
	}
	return fib(n-1) + fib(n-2)
}

func main() {
	n, _ := strconv.Atoi(os.Args[1])
	count, _ := strconv.Atoi(os.Args[2])
	for i := 0; i < count; i++ {
		fmt.Println(fib(n))
	}
}

all: main

clean:
	rm -f main

main: src/main.cpp
	clang++ -std=c++17 -O3 -Wall -Wextra -pedantic-errors -march=native -mtune=native -I external_projects/unordered_dense/include -o $@ $<

.PHONY: all clean

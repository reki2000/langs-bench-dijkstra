all: main

clean:
	rm -f main

main: src/main.cpp
	g++ -std=c++20 -O3 -Wall -Wextra -pedantic-errors -march=native -mtune=native -I external_projects/unordered_dense/include -o $@ $<

.PHONY: all clean

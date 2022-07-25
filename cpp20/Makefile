all: main

clean:
	rm -f main

INCLUDE_BOOST_CONTAINER = \
	-I external_projects/container/include \
	-I external_projects/move/include \
	-I external_projects/intrusive/include \
	-I external_projects/config/include \
	-I external_projects/assert/include \
	-I external_projects/static_assert/include \
	-I external_projects/core/include \
	-Wno-stringop-overread

main: src/main.cpp
	g++ -std=c++20 -O3 -Wall -Wextra -pedantic-errors -march=native -mtune=native -I external_projects/unordered_dense/include -I external_projects/fast_io/include -DFAST_IO_OPTIMIZE_SIZE $(INCLUDE_BOOST_CONTAINER) -o $@ $<

.PHONY: all clean

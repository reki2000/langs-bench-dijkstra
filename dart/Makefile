SRC = lib

all: bin/main

clean:
	rm -f bin/main

bin/main: $(SRC)/main.dart
	mkdir -p bin
	dart compile exe -o $@ $<

run: bin/main
	bin/main

.PHONY: all clean run

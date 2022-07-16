SRC = lib

all: main

clean:
	rm bin/main

main: $(SRC)/main.dart
	mkdir -p bin
	dart compile exe -o bin/main ${SRC}/main.dart 

run: bin/main
	bin/main

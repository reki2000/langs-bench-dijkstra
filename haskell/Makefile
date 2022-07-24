OBJ=./bin/main

all: $(OBJ)

$(OBJ):
	mkdir -p ./bin
	cabal build --copy-bins --local-bin-path ./bin

clean:
	rm -f $(OBJ)

.PHONY: all clean

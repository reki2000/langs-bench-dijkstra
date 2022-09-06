EXEDIR=./bin

all:
	mkdir -p $(EXEDIR)
	cabal install -O2 --overwrite-policy=always --installdir $(EXEDIR)

clean:
	rm -rf $(EXEDIR)
	cabal clean

test:
	cabal test

.PHONY: all clean test

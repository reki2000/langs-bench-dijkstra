EXEDIR=./bin
EXE=$(EXEDIR)/main

all: $(EXE)

$(EXE): app/Main.hs app/Graph.hs app/MapLoader.hs
	mkdir -p $(EXEDIR)
	cabal install -O2 --overwrite-policy=always --installdir $(EXEDIR)

clean:
	rm -rf $(EXEDIR)

.PHONY: all clean

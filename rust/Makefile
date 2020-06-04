all: main

clean:
	rm main

main: src/main.rs
	rustc -C opt-level=3 src/main.rs

run: main
	./main ${N} ${COUNT}

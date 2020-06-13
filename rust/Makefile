all: main

clean:
	rm main
	rm -rf target

main: src/main.rs
	rustc -C opt-level=3 src/main.rs

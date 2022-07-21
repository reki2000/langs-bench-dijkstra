all: main

clean:
	cargo clean
	rm -f main

main: src/main.rs
	cargo build --release
	cp -p ./target/release/langs-bench-rust ./$@

.PHONY: all clean

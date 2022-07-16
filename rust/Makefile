all: main

clean:
	cargo clean
	rm -f main || true

main: src/main.rs
	cargo build --release
	cp -p ./target/release/langs-bench-rust ./main

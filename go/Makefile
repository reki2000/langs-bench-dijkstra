all: main

clean:
	rm main

main: src/main.go
	go build src/main.go

run: main
	./main ${N} ${COUNT}
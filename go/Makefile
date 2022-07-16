all: main

clean:
	rm -f main || true

main: src/main.go src/priorityqueue.go
	go build src/main.go src/priorityqueue.go

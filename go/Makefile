all: main

clean:
	rm -f main

main: src/main.go src/priorityqueue.go
	go build $^

.PHONY: all clean

all: main

clean:
	rm main

main: src/main.cpp
	g++ -O3 -o main src/main.cpp

run: main
	./main $(N) $(COUNT)

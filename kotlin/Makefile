SRC = src/main/kotlin/main

all: main.jar

clean:
	rm -f main.jar || true

main.jar: $(SRC)/main.kt $(SRC)/priorityqueue.kt
	kotlinc $(SRC)/main.kt $(SRC)/priorityqueue.kt -include-runtime -d main.jar

run: main.jar

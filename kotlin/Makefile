SRC = src/main/kotlin/main

all: main.jar

clean:
	rm -f main.jar

main.jar: $(SRC)/main.kt $(SRC)/priorityqueue.kt
	kotlinc $^ -include-runtime -d $@

run: main.jar
	java -jar $<

.PHONY: all clean run

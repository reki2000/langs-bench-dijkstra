all: main.jar

clean:
	rm main.jar

main.jar: src/main.kt
	kotlinc src/main.kt -include-runtime -d main.jar

run: main.jar
	java -jar main.jar ${N} ${COUNT}

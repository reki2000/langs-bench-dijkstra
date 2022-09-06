all: src/main.jl src/priorityqueue.jl

compile: main

clean:
	rm -rf out main

main: src/main.jl src/priorityqueue.jl
	mkdir -p out
	# julia --output-jl out/main.jl src/main.jl
	julia --output-o out/main.o --sysimage ${ASDF_DIR}/installs/julia/1.4.1/julia/lib/julia/sys.so --startup-file=no $<
	cc -o $@ out/sys.o -ljulia

.PHONY: all compile clean

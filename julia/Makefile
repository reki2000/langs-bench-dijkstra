all: src/main.jl src/priorityqueue.jl

compile: src/main.jl src/priorityqueue.jl
	mkdir -p out
	# julia --output-jl out/main.jl src/main.jl
	julia --output-o out/main.o --sysimage /home/reki/.asdf/installs/julia/1.4.1/julia/lib/julia/sys.so --startup-file=no src/main.jl
	cc -o main out/sys.o -ljulia

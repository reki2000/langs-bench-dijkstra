all: main

clean:
	cd src && rm cy_g.c cy_g.*.so

main: src/cy_g.pyx
	cd src && python3 setup.py build_ext --inplace 

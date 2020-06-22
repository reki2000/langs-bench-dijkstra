all: main

clean:
	cd src && rm cy_g.c cy_g.*.so

main: src/cy_g.pyx
	pip install -r requirements.txt
	cd src && python3 setup.py build_ext --inplace 

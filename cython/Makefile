all: main

clean:
	cd src && rm -f cy_g.c cy_g.*.so || true

main: src/cy_g.pyx
	pip install -r requirements.txt
	cd src && python3 setup.py build_ext --inplace 

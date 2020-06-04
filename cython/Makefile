SO = src/main.cpython-38-darwin.so

all: $(SO)

clean:
	rm $(SO)

$(SO): src/main.pyx src/setup.py
	cd src; python setup.py build_ext --inplace

run: $(SO)
	cd src; python run.py ${N} ${COUNT}

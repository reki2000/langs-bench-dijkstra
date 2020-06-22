from distutils.core import setup
from Cython.Build import cythonize

ext = cythonize('cy_g.pyx')
setup(ext_modules=ext)

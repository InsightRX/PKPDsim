#!/usr/bin/env python3
"""
Setup script for PKPDsim Python package
"""

from setuptools import setup, find_packages, Extension
from Cython.Build import cythonize
import numpy
import os

# Read the contents of README file
this_directory = os.path.abspath(os.path.dirname(__file__))
with open(os.path.join(this_directory, 'README.md'), encoding='utf-8') as f:
    long_description = f.read()

# Define extensions for C++ integration if needed
extensions = []

# Optionally add Cython extensions for performance-critical parts
# extensions = [
#     Extension(
#         "pkpdsim.sim_core",
#         ["pkpdsim/sim_core.pyx"],
#         include_dirs=[numpy.get_include()],
#         language="c++",
#     )
# ]

setup(
    long_description=long_description,
    long_description_content_type='text/markdown',
    ext_modules=cythonize(extensions) if extensions else [],
    include_dirs=[numpy.get_include()] if extensions else [],
)
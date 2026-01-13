"""
japhrase - Japanese phrase extraction package

This setup.py is a compatibility shim.
All configuration is in pyproject.toml

To install in development mode:
    pip install -e .

To build:
    pip install build
    python -m build
"""

from setuptools import setup, find_packages

setup(
    packages=find_packages(),
)

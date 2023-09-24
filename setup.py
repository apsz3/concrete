from setuptools import setup

setup(
    name="concrete",
    version="0.0.1",
    packages=["concrete"],
    entry_points={"console_scripts": ["ccr = concrete.cli:cli"]},
)

import setuptools
with open("README.md", "r", encoding='utf-8') as fh:
    long_description = fh.read()

setuptools.setup(
    name="jphrase",
    version="0.1.0",
    author="Takeshi SHIMIZU",
    author_email="shim1zu@hotmail.com",
    description="You can detect new phrases or unknown words for texts in Japanese",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/tshim1zu/phrase-project",
    packages=setuptools.find_packages(),
    classifiers=[
        "Programming Language :: Python :: 3",
        "Operating System :: OS Independent",
    ],
    entry_points = {
    },
    python_requires='>=3.7',
)
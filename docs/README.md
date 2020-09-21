# Documentation

Product documentation is built using [Sphinx docs](https://www.sphinx-doc.org/)
and written in [reStructured Text](https://docutils.readthedocs.io/en/sphinx-docs/user/rst/quickstart.html) markup language (reST).

## Building with Sphinx

From docs folder run the following commands. To install the requirements needed
for generating the docs:

```
pip install -r requirements.txt
```

Afterwards you can run:

```
make html
```

The docs will be generated, and the output files placed in the `_build/html/`
directory, which can be browsed (locally) with any browser.

The docs can be found online at [https://accelerate.readthedocs.org](https://accelerate.readthedocs.org).


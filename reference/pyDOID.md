# pyDOID

[pyDOID](https://github.com/allenbaron/pyDOID) is a python package
developed with the same intent as `DO.utils`, to provide methods for
updating, maintaining, organizing, and analyzing the Human Disease
Ontology. `DO.utils`, powered by
[reticulate](https://github.com/rstudio/reticulate), wraps `pyDOID` for
functionality that is currently unavailable in R. `pyDOID` provides two
primary classes: the `DOrepo` class and the `owl.xml` class. Classes and
their methods are accessible via `$` notation.

## Version

Documentation assumes pyDOID \>= v0.1.2

## DOrepo class

The `DOrepo` class inherits the methods & subclasses of the
[GitPython.repo.Repo](https://gitpython.readthedocs.io/en/stable/tutorial.html#meet-the-repo-type)
class and adds a few additional classes & methods, see
[`DOrepo()`](https://allenbaron.github.io/DO.utils/reference/DOrepo.md)
for details.

## owl.xml class

The `owl.xml` class thinly wraps a small set of python
[rdflib](https://rdflib.readthedocs.io/en/stable/) methods to enable
SPARQL 1.1 queries, see
[`owl_xml()`](https://allenbaron.github.io/DO.utils/reference/owl_xml.md)
for details.

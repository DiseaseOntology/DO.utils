#' pyDOID
#'
#' [pyDOID](https://github.com/allenbaron/pyDOID) is a python package
#' developed with the same intent as `DO.utils`, to provide methods for
#' updating, maintaining, organizing, and analyzing the Human Disease Ontology.
#' `DO.utils`, powered by [reticulate](https://github.com/rstudio/reticulate),
#' wraps `pyDOID` for functionality that is currently unavailable in R.
#' `pyDOID` provides two primary classes: the `DOrepo` class and the `owl.xml`
#' class. Classes and their methods are accessible via `$` notation.
#'
#' @section Version:
#' Documentation assumes pyDOID >= v0.1.2
#'
#' @section DOrepo class:
#' The `DOrepo` class inherits the methods & subclasses of the
#' [GitPython.repo.Repo](https://gitpython.readthedocs.io/en/stable/tutorial.html#meet-the-repo-type)
#' class and adds a few additional classes & methods, see [DOrepo()] for
#' details.
#
#' @section owl.xml class:
#' The `owl.xml` class thinly wraps a small set of python
#' [rdflib](https://rdflib.readthedocs.io/en/stable/) methods to enable SPARQL
#' 1.1 queries, see [owl_xml()] for details.
#'
#' @name pyDOID
NULL


#' Instantiate a DO Repository
#'
#' Instantiate a `DOrepo` object from the
#' [pyDOID](https://github.com/allenbaron/pyDOID) package (powered by
#' [reticulate](https://github.com/rstudio/reticulate)).
#'
#' @section Classes & Methods:
#' The `DOrepo` class inherits the methods & subclasses of the
#' [GitPython.Repo](https://gitpython.readthedocs.io/en/stable/tutorial.html#meet-the-repo-type)
#' class, along with the following:
#'
#' * `DOrepo$tag_iterate(function, start = [str], end = [str])`: Iterate
#' through DO releases (tags) from `start` to `end` executing a `function` at
#' each. `start` and `end` are _optional_; if unspecified the first or last
#' release/tag of the repo will be assumed.
#'
#' * `DOrepo$capture_head()`: Capture the current `head` of the repo; useful
#' for restoring state after git checkout(s). _USE_: If assigned to variable
#' `x`, restore state with `x$checkout()`.
#'
#' * `DOrepo$doid` and `DOrepo$doid_merged`: Access the doid.owl and
#' doid-merged.owl files of the repository. These inherit from the
#' `pyDOID.owl.xml` class, see [owl_xml()] for methods. _NOTE_ that, unlike
#' [owl_xml()], doid.owl and doid-merged.owl are not loaded into memory upon
#' instantiation of `DOrepo`, but are loaded automatically when the first
#' `$query()` is executed or manually via `$load()`.
#'
#' @param path The local path to the
#' [HumanDiseaseOntology](https://github.com/DiseaseOntology/HumanDiseaseOntology)
#' git repository, as a string.
#'
#' @family pyDOID classes
#' @export
DOrepo <- function(path) {
    pyDOID$DOrepo(path)
}


#' Instantiate an owl_xml object
#'
#' Instantiate a python `owl.xml` object from the
#' [pyDOID](https://github.com/allenbaron/pyDOID) package (powered by
#' [reticulate](https://github.com/rstudio/reticulate)). The file will be loaded
#' into memory when instantiated. Though named "owl" this method will also work
#' for RDF files.
#'
#' @section Classes & Methods:
#' The `owl.xml` class thinly wraps a small set of python
#' [rdflib](https://rdflib.readthedocs.io/en/stable/) methods to enable SPARQL
#' 1.1 queries, with the following methods:
#'
#' * `owl_xml$query(query, reload)`: Execute a SPARQL 1.1 query. `query` may be
#' a string or the path to a .sparql/.rq file. Use `reload = TRUE` to force
#' reload a file into memory; useful when iterating through releases/tags with
#' `DOrepo$tag_iterate()`.
#'
#' * `owl_xml$load()`: Manually load a file into memory. Generally, this should
#' not be necessary.
#'
#' @param path The path to an OWL (RDF) XML-formatted file, as a string.
#'
#' @family pyDOID classes
#' @export
owl_xml <- function(path) {
    pyDOID$owl$xml(path)$load()
}

#' Access OWL/RDF XML (INTERNAL)
#'
#' Access `x` as a 'pyDOID.owl.xml' by confirming it's class, if already
#' instantiated, or instantiating the object from a given path.
#'
#' @param x A 'pyDOID.owl.xml' object or the path to an OWL/RDF XML file that
#'     can be instantiated as such an object by [owl_xml()].
#'
#' @keywords internal
access_owl_xml <- function(x) {
    if (is_owl_xml(x)) {
        return(x)
    }

    owl_xml(x)
}

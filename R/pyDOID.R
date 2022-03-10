#' Instantiate a DO Repository
#'
#' Instantiates a `DOrepo` python object from the
#' [pyDOID](https://github.com/allenbaron/pyDOID) package (powered by
#' [reticulate](https://github.com/rstudio/reticulate)).
#'
#'
#' The `DOrepo` class inherits the methods & subclasses (accessible via `$`
#' notation) of the
#' [GitPython.Repo](https://gitpython.readthedocs.io/en/stable/tutorial.html#meet-the-repo-type)
#' class, and adds the following:
#'
#' * `DOrepo$tag_iterate(function, start, end)`: Iterates through DO releases
#' (tags) from `start` to `end` executing a `function` at each. If `start` or
#' `end` are not specified the first or last release/tag of the repo will be
#' assumed.
#'
#' * `DOrepo${doid|doid_merged}$query(query, reload)`: Executes a SPARQL
#' `query`, input as a string or file path, against the specified DO file. Will
#' optionally `reload` the file into memory, useful in conjunction with
#' `tag_iterate()`.
#'
#' * `DOrepo$capture_head()`: Captures the current `head` of the repo, useful
#' for restoring state after git checkout(s). Use: If assigned to variable
#' `x`, restore state with `x$checkout()`.
#'
#' @param path The local path to the
#' [HumanDiseaseOntology](https://github.com/DiseaseOntology/HumanDiseaseOntology)
#' repository, as a string.
#'
#' @name pyDOID
#' @export
DOrepo <- function(path) {
    pyDOID$DOrepo(path)
}


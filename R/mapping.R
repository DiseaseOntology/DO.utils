#' Map Terms to Terms of Specified Namespace
#'
#' Map `term` to terms of a specified namespace (e.g. "DOID") using
#' [PyOBO](https://github.com/pyobo/pyobo) (and by dependency other
#' [biopragmatics](https://biopragmatics.github.io/) and
#' [INDRA labs](https://indralab.github.io/) programs; e.g.
#' [GILDA](https://github.com/indralab/gilda),
#' [bioregistry](http://bioregistry.io/)). In INDRA labs
#' terminology, GILDA grounds (predicts matches of) `term` to `namespace` terms.
#'
#' @param terms Term(s) to map, as a character vector.
#' @param namespace Namespace to map terms to (_according to bioregistry_), as a
#'     string.
#'
#' @returns
#' List of results for each term. See [parse_mapping()] and dependencies for
#' details on format of results.
#'
#' @export
pyobo_map <- function(terms, namespace) {
    pyobo_gutils <- reticulate::import("pyobo.gilda_utils")
    grounder <- pyobo_gutils$get_grounder(namespace)
    res <- purrr::map(terms, grounder$ground) %>%
        purrr::set_names(terms)

    res
}

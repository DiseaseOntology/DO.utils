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

#' Extract ScoredMatch Objects (INTERNAL)
#'
#' Extracts mapping results from specialized `ScoredMatch` python objects
#' (as defined by GILDA) AT THE LEVEL of individual mappings.
#'
#' @param py_ScoredMatch A GILDA ScoredMatch (python) object.
#' @param prefix _Optional_ prefix to add to namespace local unique identifiers
#'     (LUI; e.g. 4, the LUI for "disease" in DO), as a string; preferably to
#'     create a complete namespace ID (e.g. "DOID:4").
#' @param prefix_sep _Optional_ separator placed between `prefix` and
#'     namespace LUIs, as a string. Ignored if `prefix = NULL`.
extract_ScoredMatch <- function(py_ScoredMatch, prefix = NULL,
                                prefix_sep = ":") {
    if (is.null(prefix)) {
        prefix_sep <- NULL
    }

    df <- tibble::tibble(
        id = paste0(prefix, prefix_sep, py_ScoredMatch$term$id),
        term = py_ScoredMatch$term$entry_name,
        score = round(py_ScoredMatch$score, 2)
    )

    df
}

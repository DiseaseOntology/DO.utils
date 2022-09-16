# to make where available, since it's not exported from tidyselect yet
#   will be in next version > 1.1.2
utils::globalVariables("where")

#' Tidy SPARQL Query
#'
#' Tidies SPARQL query results, unnesting list columns and returning results as
#' a [tibble](tibble::tibble()) instead of a data.frame.
#'
#' @param query_res The results of a SPARQL query, as a data.frame (usually
#'     produced by [owl_xml()$query()](owl_xml()) or
#'     [DOrepo()$doid{_merged}?$query()](DOrepo())).
#'
#' @section Note:
#' This function exists because the results are not currently tidied by `pyDOID`
#' (the python package that provides SPARQL query functionality to `DO.utils`).
#'
#' @export
tidy_sparql <- function(query_res) {
    query_res %>%
        tibble::as_tibble() %>%
        DO.utils::unnest_cross(where(is.list), keep_empty = TRUE)
}


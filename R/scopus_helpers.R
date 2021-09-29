# classes

ss_class <- c("scopus_search", "list")
ss_list_class <- c("scopus_search_list", "list")


# internal functions

#' Create a REFTITLE query
#'
#' Creates a single quoted REFTITLE (reference title) query for the Scopus
#' Search API. Submitting multiple titles will result in each individually
#' wrapped by REFTITLE() and all concatenated by OR.
#'
#' To obtain results for multiple REFTITLEs separately, use
#' `scopus_title_query` once for each.
#'
#' @noRd
scopus_title_query <- function(title) {

    if (length(title) == 1) {
        q <- paste0("REFTITLE(\"", title, "\")")
    } else {
        q <- DO.utils::vctr_to_string(
            paste0("REFTITLE(\"", title, "\")"),
            delim = " OR "
        )
    }

    q
}

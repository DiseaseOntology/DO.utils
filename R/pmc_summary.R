#' Get PubMed Central Summary
#'
#' Retrieves PubMed Central Summary information for specified publications.
#' Acts in exactly the same manner as [pubmed_summary()]. _Consult_
#' _documentation there for more details._
#'
#' @param input One of the following:
#'     1. A vector with unique PubMed Central IDs.
#'     2. A list of vectors with unique PubMed Central IDs.
#'     3. A `web_history` object (see NCBI Entrez API documentation for
#'     information).
#' @inheritParams pubmed_summary
#'
#' @export
pmc_summary <- function(input, config = NULL, version = "2.0",
                           retmode = "xml", ...) {

    if ("web_history" %in% class(input)) {
        web_history <- input
        id <- NULL
    } else if (purrr::is_list(input)) {
        # minimize summary request (limit to unique PMIDs)
        web_history <- NULL
        id <- unique(unlist(input))
    } else {
        web_history <- NULL
        id <- input
    }

    # strip PMC from identifier, if present (API uses bare number)
    id <- stringr::str_remove(id, "^PMC")

    if (is.null(web_history) & length(id) > 200) {
        web_history <- rentrez::entrez_post("pmc", id = id)
        id <- NULL
    }

    summary_res <- rentrez::entrez_summary(
        db = "pmc",
        id = id,
        web_history = web_history,
        version = version,
        always_return_list = TRUE,
        retmode = retmode,
        config = config,
        ...
    )

    if (purrr::is_list(input)) {
        summary_list <- purrr::map(
            input,
            ~ summary_res[.x]
        )
        class(summary_list) <- "esummary_list_nested"
    } else {
        summary_list <- summary_res
    }

    summary_list
}

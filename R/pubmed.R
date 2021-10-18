#' Get PubMed Summary
#'
#' Retrieves PubMed Summary information for specified publications. Acts as a
#' simple wrapper around [rentrez::entrez_summary] to allow larger number of
#' IDs as input (using [rentrez::entrez_post]).
#'
#' @inheritParams rentrez::entrez_summary
#' @param version,always_return_list preferred defaults are set but
#' included here for flexibility; see [rentrez::entrez_summary()] for details
#' @param retmode "xml" (default) or "json"; "xml" is preferred because of a
#' higher limit. This default is opposite the [rentrez::entrez_summary()]
#' default.
#'
#' @export
pubmed_summary <- function(id = NULL, web_history = NULL, config = NULL,
                           version = "2.0", retmode = "xml",
                           always_return_list = TRUE, ...) {

    if (is.null(web_history) & length(id) > 200) {
        web_history <- rentrez::entrez_post("pubmed", id = id)
        id <- NULL
    }

    pm_summary <- rentrez::entrez_summary(
        db = "pubmed",
        id = id,
        web_history = web_history,
        version = version,
        always_return_list = always_return_list,
        retmode = retmode,
        config = config,
        ...
    )

    pm_summary
}


#' Truncate PubMed Author List (internal)
#'
#' Truncate Authors for any PubMed article to 120 and list the number of
#' additional authors in the subsequent Author position.
#'
#' @section NOTE:
#' The number of authors retained was determined based on JSON string conversion
#' length and Google Sheets cell character limit. The maximum string conversion
#' of authors to json is ~ 400 / author (mean ~ 290). Google Sheets character
#' limit / cell = 50,000. 120 authors is a conservative limit that's unlikely
#' to cause problems (and is plenty long).
#'
#' @param pubmed_df A tidy data frame, as produced by [tidy()] on a PubMed
#' `esummary_list`
#'
truncate_authors <- function(pubmed_df) {
    dplyr::mutate(
        pubmed_df,
        Authors = purrr::map(
            pubmed_df$Authors,
            function(author_list) {
                n <- length(author_list)
                if (n > 120) {
                    author_list[[121]][1] <- paste0(
                        "+ ", n - 120, " additional authors"
                    )
                    author_list[[121]][2:length(author_list[[121]])] <- NULL
                    author_list[122:n] <- NULL
                }
                author_list
            }
        )
    )
}

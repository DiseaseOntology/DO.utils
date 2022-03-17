#' Get PubMed Summary
#'
#' Retrieves PubMed Summary information for specified publications. Acts as a
#' wrapper around [rentrez::entrez_summary] to allow a larger number of
#' IDs as input (using [rentrez::entrez_post]) and for input as an ID list (in
#' addition to an `id` vector or `web_history` object).
#'
#' @param input One of the following: 1) A vector with unique PubMed IDs, 2) a
#'     list of vectors with PubMed IDs (for example, output from
#'     `citedby_pmid(.... by_id = TRUE)` > `extract_pmid()`), OR 3) a
#'     `web_history` object (see NCBI Entrez API documentation for information).
#' @param version Argument included here for flexibility,
#'     but best left with the defaults as set; see [rentrez::entrez_summary()]
#'     for details.
#' @param retmode "xml" (default) or "json"; "xml" is preferred because of a
#'     higher response limit. This default is opposite the
#'     [rentrez::entrez_summary()] default.
#' @inheritParams rentrez::entrez_summary
#'
#' @export
pubmed_summary <- function(input, config = NULL, version = "2.0",
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

    if (is.null(web_history) & length(id) > 200) {
        web_history <- rentrez::entrez_post("pubmed", id = id)
        id <- NULL
    }

    pm_summary_res <- rentrez::entrez_summary(
        db = "pubmed",
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
            function(i) {
                res <- pm_summary_res[i]
                class(res) <- class(pm_summary_res)
                res
            }
        )
        class(summary_list) <- "esummary_list_nested"
    } else {
        summary_list <- pm_summary_res
    }

    summary_list
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


#' Hoists IDs from Pubmed/PMC Data (Internal)
#'
#' Hoists IDs from Pubmed/PMC Data. _Does not use [tidyr::hoist()] because_
#' _`IdType` identifier is not a parent and ID positions in list can be_
#' _variable._
#'
#' @section Note:
#' In direct results from PubMed/PMC the `ArticleIds` results are not
#' equivalent. The ID type names from PMC correspond with those in this package
#' but the PubMed ID type names do not. For PubMed `pmcid` is named `pmc` and
#' `pmid` is named `pubmed`.
#'
#' @param pubmed_df A data frame, as produced by [as_tibble()] on a PubMed/PMC
#'     `esummary_list`.
#' @param id One or more IDs to hoist, as a character vector. If `NULL`
#'     (default), all IDs will be hoisted. Available IDs may include "doi",
#'     "eid", "mid", "pii", "pmcid", "pmcid_long", "pmid", or "rid".
#'
hoist_ArticleIds <- function(pubmed_df, id = NULL) {

    id_df <- purrr::map(pubmed_df$ArticleIds, tidy_ArticleId_set) %>%
        dplyr::bind_rows()

    # rename to match pkg internal representation; only needed for PubMed
    if (all(c("pmcid", "pmc") %in% names(id_df))) {
        id_df <- id_df %>%
            dplyr::rename(
                pmcid_long = .data$pmcid,
                pmcid = .data$pmc,
                pmid = .data$pubmed
            )
    }

    if (is.null(id)) {
        out_df <- dplyr::bind_cols(pubmed_df, id_df)
    } else {
        out_df <- dplyr::bind_cols(
            pubmed_df,
            dplyr::select(id_df, {{ id }})
        )
    }
    out_df
}


#' Convert ArticleId list to Data Frame (Internal)
#'
#' Converts a single set of ArticleId objects into a data frame. These are
#' confined to individual rows in PubMed ArticleIds list columns produced by
#' [as_tibble.esummary_list()].
#'
#' @param x A single set of PubMed ArticleId objects from `esummary_list`
#'     as contained in rows after after conversion with [as_tibble()].
#'
#' @noRd
tidy_ArticleId_set <- function(x) {
    purrr::map_dfr(
        x,
        ~ tibble::tibble(
            type = .x$IdType,
            #type_N = .x$IdTypeN, # not needed
            value = .x$Value
        )
    ) %>%
        tidyr::pivot_wider(names_from = .data$type, values_from = .data$value)
}

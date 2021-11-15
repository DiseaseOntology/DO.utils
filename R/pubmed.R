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
            ~ pm_summary_res[.x]
        )
        class(summary_list) <- "esummary_list_nested"
    } else {
        summary_list <- pm_summary_res
    }

    summary_list
}


#' Read in PubMed Citations (from txt file)
#'
#' Reads in and concatenates PubMed text-format citations (span multiple
#' lines, usually obtained by downloading a text file from PubMed as
#' 'Summary (text)).
#'
#' @param file Path to .txt file; or another possible input to `readLines()`.
#'
#' @export
read_pubmed_txt <- function(file) {
    txt_citations <- readLines(file) %>%
        stringr::str_trim()

    # identify spacer after each citation & citation start/end locations
    spacer_loc <- c(
        stringr::str_which(txt_citations, "^$"),
        length(txt_citations)
    )
    start_loc <- c(1, stats::na.omit(dplyr::lag(spacer_loc)) + 1)
    end_loc <- spacer_loc - 1

    # collapse citations
    citations <- purrr::map2_chr(
        .x = start_loc,
        .y = end_loc,
        ~ paste0(txt_citations[.x:.y], collapse = " ")
    )

    # remove empty citations
    citations <- citations[!stringr::str_detect(citations, "^[NA ]+$")] %>%
        # remove extra whitespace
        stringr::str_squish()

    citations
}


#' Extract Publication Date from PubMed Citations
#'
#' Extracts most complete publication date possible from Pubmed citations.
#'
#' This function uses a step-wise approach, attempting first to extract a full
#' date, subsequently a year & month and, if that is not available, just the
#' year. This approach is designed to prevent accidental matches to year values
#' found in titles.
#'
#' NOTE: When the day is missing, this function will return a full date using
#' the first day of the month. When both the month and day are missing, this\
#' function will return the first day of the year.
#'
#' @param citation character vector of PubMed citations
#'
#' @md
#' @export
extract_pm_date <- function(citation) {

    # define regex patterns
    ymd_regex <- "[12][0-9]{3} (Jan|Feb|Ma[ry]|Apr|Ju[nl]|Aug|Sep|Oct|Nov|Dec) [0-9]{1,2}"
    ym_regex <- "[12][0-9]{3} (Jan|Feb|Ma[ry]|Apr|Ju[nl]|Aug|Sep|Oct|Nov|Dec)"
    y_regex <- "[12][0-9]{3}"

    # stepwise identification to avoid picking up dates from titles, as much
    #   as possible
    pub_date <- dplyr::case_when(
        stringr::str_detect(citation, ymd_regex) ~
            lubridate::ymd(stringr::str_extract(citation, ymd_regex)),
        # if lacking day, will use first day of month
        stringr::str_detect(citation, ym_regex) ~
            lubridate::ym(stringr::str_extract(citation, ym_regex)),
        stringr::str_detect(citation, y_regex) ~
            # if year only, not ideal (use first day of year)
            lubridate::ymd(
                paste0(
                    stringr::str_extract(citation, y_regex),
                    "-01-01"
                )
            )
    )

    pub_date
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


#' Hoists IDs from Pubmed Data (Internal)
#'
#' Hoists IDs from Pubmed Data. _Does not use [tidyr::hoist()] because `IdType`_
#' _identifier is not a parent and ID positions in list can be variable._
#'
#' @param pubmed_df A data frame, as produced by [as_tibble()] on a PubMed
#'     `esummary_list`.
#' @param id One or more IDs to hoist, as a character vector. If `NULL`
#'     (default), all IDs will be hoisted. Available IDs may include "doi",
#'     "eid", "mid", "pii", "pmcid", "pmcid_long", "pmid", or "rid".
#'
#' @noRd
hoist_ArticleIds <- function(pubmed_df, id = NULL) {

    id_df <- purrr::map(pubmed_df$ArticleIds, tidy_ArticleId_set) %>%
        dplyr::bind_rows() %>%
        # rename to match pkg internal representation
        dplyr::rename(pmcid_long = pmcid, pmcid = pmc, pmid = pubmed)

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
        tidyr::pivot_wider(names_from = type, values_from = value)
}

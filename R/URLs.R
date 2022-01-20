read_doid_edit <- function(DO_repo) {
    doid_edit_path <- file.path(DO_repo, "src", "ontology", "doid-edit.owl")
    doid_edit <- readr::read_lines(doid_edit_path)

    doid_edit
}

extract_doid_url <- function(doid_edit, w_raw_match = FALSE, quiet = FALSE,
                             show_url_string = FALSE) {
    doid_w_url <- doid_edit[has_doid_url(doid_edit)]

    df <- tibble::tibble(
        raw_match = doid_w_url,
        doid = stringr::str_extract_all(doid_w_url, "DOID[:_][0-9]+"),
        url_str = stringr::str_extract_all(doid_w_url, 'url:[^"]+"')
    )

    # warn if multiple DOIDs associated with record (should only be 1)
    doid_mult <- purrr::map_int(df$doid, length) > 1L
    if (any(doid_mult) && !quiet) {
        rlang::warn(
            message = c(
                "Entries are associated with multiple DOIDs:",
                i = doid_w_url[doid_mult]
            ),
            class = "unexpected_value"
        )
    }

    # tidy
    df <- df %>%
        tidyr::unnest_longer(.data$doid) %>%
        tidyr::unnest_longer(.data$url_str) %>%
        dplyr::mutate(
            doid = stringr::str_replace(.data$doid, ".*DOID[_:]", "DOID:"),
            url = stringr::str_remove_all(.data$url_str, '^url:|"')
        )

    if (!isTRUE(show_url_string)) {
        df <- dplyr::select(df, -.data$url_str)
    }

    if (!isTRUE(w_raw_match)) {
        df <- dplyr::select(df, -.data$raw_match)
    }

    df
}

has_doid_url <- function(doid_edit) {
    grepl("DOID", doid_edit) & grepl("url:", doid_edit)
}


#' Get URL (internal)
#'
#' Get a URL used within this package. Available URLs: "doi", "pubmed", "pmc",
#' "pmc_article" (append as prefix to article pmcid for direct navigation),
#' "alliance_disease_tsv"
#'
#' @param .name internal name of desired URL
get_url <- function(.name) {
    .name <- match.arg(
        .name,
        c("doi", "pubmed", "pmc", "pmc_article", "alliance_disease_tsv")
    )

    switch(
        .name,
        doi = "https://www.doi.org/",
        pubmed = "https://pubmed.ncbi.nlm.nih.gov/",
        pmc = "https://www.ncbi.nlm.nih.gov/pmc/",
        pmc_article = "https://www.ncbi.nlm.nih.gov/pmc/articles/",
        alliance_disease_tsv = "https://fms.alliancegenome.org/download/DISEASE-ALLIANCE_COMBINED.tsv.gz"
    )
}

#' Append to URL
#'
#' Append a value to a URL.
#'
#' @section Note:
#' No URL validation is performed.
#'
#' @param x value to append
#' @param url a URL or the internal name of a URL used in this package (see
#' [get_url] for possible names)
#'
#' @export
append_to_url <- function(x, url) {

    url <- tryCatch(get_url(url), error = function(e) url)

    # add '/' if no terminal '/' in URL
    if (stringr::str_detect(url, "/$")) {
        new_url <- paste0(url, x)
    } else {
        new_url <- paste0(url, "/", x)
    }

    new_url
}

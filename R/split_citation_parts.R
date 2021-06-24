# Split citation info -----------------------------------------------------

quiet_separate <- purrr::quietly(tidyr::separate)

try_separate <- function(...) {
    tryCatch(
        tidyr::separate(...),
        warning = function(w) {
            str(w)
        }
    )
}

parse_citation <- function(x) {
    cite_df <- tibble::tibble(citation = x)

    parsed_df <- cite_df %>%
        try_separate(
            citation,
            into = c("id", "authors", "title", "journal", "article_id",
                     "doi", "etc"),
            sep = "\\. ",
            remove = FALSE,
            extra = "merge",
            fill = "warn"
        )

    parsed_df
}

t3 <- parse_citation(citations)

tidyr::extract(
    citation,
    into = c("id", "authors", "title", "journal", "article_id",
             "doi", "pmid", "pmcid"),
    regex = "^([0-9]+)\\. ([^.]+)\\. ([^.]+)\\.

#' Concatenate PubMed Citations (from txt file)
#'
#' Concatenates citations spanning multiple lines, usually obtained
#' by downloading a text file from PubMed
#'
#' @param file path to .txt file; or another possible input to `readLines()`
#'
#' @md
#' @export
concat_pm_citation <- function(file) {
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
    ) %>%
        # remove empty citations
        {.[!stringr::str_detect(., "^[NA ]+$")]} %>%
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

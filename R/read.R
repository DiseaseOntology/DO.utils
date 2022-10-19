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

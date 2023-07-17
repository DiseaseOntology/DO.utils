#' Read in PubMed Citations (from txt file)
#'
#' Reads PubMed text-format citations spanning multiple lines, usually
#' obtained by downloading a text file from PubMed as 'Summary (text)'.
#'
#' @param file Path to .txt file; or another possible input to `readLines()`.
#'
#' @returns
#' A data.frame with a record number (`n`), identifiers (`pmid`, `pmcid`,
#' `doi`), and the full citation (`citation`).
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

    # extract identifiers and format as tibble
    citation_df <- tibble::tibble(
        n = 1:length(citations),
        pmid = stringr::str_match(citations, "PMID: ([0-9]+)")[, 2] %>%
            dplyr::na_if("0"),
        pmcid = stringr::str_match(citations, "PMCID: (PMC[0-9]+)")[, 2],
        doi = stringr::str_match(citations, "(doi|DOI): (10[^[:space:]]+)\\.?")[, 3],
        citation = citations
    )

    citation_df
}


# INTERNAL readers --------------------------------------------------------

#' Automatically Identify & Read TSV/CSV files (INTERNAL)
#'
#' A light wrapper around [readr::read_delim()] that automatically identifies
#' the delimiter based on file extension (can include compression extensions).
#'
#' Note that this function is primarily intended for internal use; therefore,
#' messages about guessed column types are not generated.
#'
#' @param file Either a path to a file, a connection, or literal data
#'    (either a single string or a raw vector).
#'
#'    Files ending in `.gz`, `.bz2`, `.xz`, or `.zip` will
#'    be automatically uncompressed. Files starting with `http://`,
#'    `https://`, `ftp://`, or `ftps://` will be automatically
#'    downloaded. Remote gz files can also be automatically downloaded and
#'    decompressed.
#' @inheritParams readr::read_delim
#' @inheritDotParams readr::read_delim -delim -quoted_na
#'
#' @keywords internal
read_delim_auto <- function(file, ..., show_col_types = FALSE) {
    ext <- stringr::str_extract(file, "\\.[tc]sv")
    delim <- switch(ext, .tsv = "\t", .csv = ",")
    if (is.null(delim)) rlang::abort("`file` must have .tsv or .csv extension.")

    readr::read_delim(
        file = file,
        delim = delim,
        show_col_types = show_col_types,
        ...
    )
}


#' Read doid-edit.owl (INTERNAL)
#'
#' Read the doid-edit.owl file from a local copy of the Human Disease Ontology
#' Github repo.
#'
#' @param DO_repo The local path to the `HumanDiseaseOntology` repo, as a
#'     string.
#'
#' @keywords internal
read_doid_edit <- function(DO_repo) {
    doid_edit_path <- file.path(DO_repo, "src", "ontology", "doid-edit.owl")
    doid_edit <- readr::read_lines(doid_edit_path)

    class(doid_edit) <- c("doid_edit", class(doid_edit))
    doid_edit
}


#' Read Manually Copied OMIMPS data
#'
#' Properly formats OMIMPS data copied or downloaded from https://omim.org/ and
#' appends the following columns to speed up curation activities:
#' * `omim`: properly formatted xref for the DO.
#' * `geno_inheritance`: best guess at inheritance to add as logical subClassOf
#' axiom.
#'
#' As part of formatting, column misarrangements are corrected and whitespace
#' is trimmed.
#'
#' @param file The path to a .tsv or .csv file (possibly compressed) with OMIMPS
#'     data.
#' @inheritDotParams preprocess_omim_dl
#'
#' @keywords internal
read_omim <- function(file, ...) {
    df <- preprocess_omim_dl(file, ...)
    df <- df %>%
        dplyr::mutate(
            omim = paste0("OMIM:", .data$phenotype_mim_number),
            geno_inheritance = dplyr::case_when(
                .data$inheritance == "AR" ~ 'autosomal recessive inheritance',
                .data$inheritance == "AD" ~ 'autosomal dominant inheritance',
                .data$inheritance == "XLR" ~ 'X-linked recessive inheritance',
                .data$inheritance == "XLD" ~ 'X-linked recessive inheritance',
                stringr::str_detect(.data$inheritance, stringr::coll("AR")) &
                    stringr::str_detect(.data$inheritance, stringr::coll("AD")) ~ 'autosomal inheritance',
                stringr::str_detect(.data$inheritance, stringr::coll("XLR")) &
                    stringr::str_detect(.data$inheritance, stringr::coll("XLD")) ~ 'X-linked inheritance',
                .default = NA_character_
            )
        ) %>%
        dplyr::mutate(
            dplyr::across(
                dplyr::where(is.character),
                ~ readr::parse_guess(.x, guess_integer = TRUE)
            )
        )

    df
}

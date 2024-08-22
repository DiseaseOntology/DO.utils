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


#' Read Google Analytics Exports
#'
#' Read exported Google Analytics data saved as `.csv`.
#'
#' @param ga_file The path to a Google Analytics `.csv` file.
#' @inheritParams readr::read_csv
#' @param read_all Whether all tables in the exported file should be read, as a
#'     logical scalar. If `FALSE` (default), only the first (main) table is
#'     read.
#' @param tidy Whether tables should be tidied with [tidy_ga_tbl()], as a
#'     logical scalar (default: `TRUE`).
#' @inheritParams tidy_ga_tbl
#' @inheritDotParams readr::read_csv -show_col_types
#'
#' @export
read_ga <- function(ga_file, read_all = FALSE, tidy = TRUE, keep_total = FALSE,
                    ...) {
    stopifnot(
        rlang::is_bool(read_all),
        rlang::is_bool(tidy)
    )
    rlang::check_installed(
        pkg = c("readr", "stringr", "purrr", "utils"),
        reason = "to use read_ga()"
    )

    .data <- readr::read_lines(ga_file)
    tbl_delim <- c(
        which(stringr::str_detect(.data, "^$")), # empty lines delimit tbls
        length(.data) + 1 # ensure last table has endpoint
    )
    if (read_all & length(tbl_delim) > 2) {
        dot_args <- list(...)
        tbl <- purrr::map2(
            utils::head(tbl_delim + 1, -1),
            utils::tail(tbl_delim - 1, -1),
            function(.x, .y) {
                out <- rlang::exec(
                    readr::read_csv,
                    file = paste(.data[.x:.y], "\n"),
                    show_col_types = FALSE,
                    !!!dot_args
                )
            }
        )

        if (tidy) {
            tbl <- purrr::map(tbl, ~ tidy_ga_tbl(.x, keep_total = keep_total))
        }
    } else {
        tbl <- readr::read_csv(
            file = paste(
                .data[(tbl_delim[1] + 1):(tbl_delim[2] - 1)],
                "\n"
            ),
            show_col_types = FALSE,
            ...
        )

        if (tidy) {
            tbl <- tidy_ga_tbl(tbl, keep_total = keep_total)
        }
    }

    tbl
}


#' Read OMIM Data
#'
#' Reads and formats OMIM data copied or manually downloaded from
#' https://omim.org/, or downloaded with [download_omim()] (permission
#' required), and appends columns to speed up subsequent curation activities.
#'
#' @section Manual Input Requirements:
#' The `file` with OMIM data copied or downloaded must include headers at the
#' top. These data can be left _as copied & pasted from omim.org_ even if they
#' are not formatted correctly, as `read_omim()` will process and correct
#' headers, which includes fixing multi-line or misarranged column headers,
#' and will trim whitespace.
#'
#' @param file The path to a file (possibly compressed) with copy/pasted or
#' manually downloaded from https://omim.org/ (see "Manual Input Requirements"
#' for details), or downloaded with [download_omim()].
#' @param keep_mim \[**OMIM search data only**\] The MIM symbols representing
#' the data types to keep, as a character vector, or `NULL` to retain all
#' (default: `"#"` and `"%"`).
#'
#' The [OMIM](https://www.omim.org/help/faq#1_3) defined MIM symbols are:
#' | MIM symbol | MIM type                            |
#' |------------|-------------------------------------|
#' | `*`        |  gene                               |
#' | `+`        |  gene, includes phenotype           |
#' | `#`        |  phenotype                          |
#' | `%`        |  phenotype, unknown molecular basis |
#' | `^`        |  deprecated                         |
#' | `none`     |  phenotype, suspected/overlap       |
#' @inheritDotParams read_delim_auto -show_col_types
#'
#' @returns An `omim_tbl` (tibble) with an `omim` column containing OMIM CURIEs
#' as formatted in DO xrefs, followed by complete OMIM data arranged as seen on
#' omim.org for OMIM **entries** (where possible). If the omim.org "Download as"
#' button was used to download the data, the `omim_tbl` will be additionally
#' modified based on the download type:
#' * Search list download: Additional `omim_search` class and `search` column
#' containing the search used.
#' * OMIM phenotypic series titles download: Additional `omim_PS_titles` class.
#' * OMIM phenotypic series download: Additional `omim_PS` class and a row
#' representing the OMIM phenotypic series itself.
#'
#' Output with columns typical OMIM phenotype entries, including `omim_PS`, will
#' have an additional `geno_inheritance` column containing a best guess at
#' inheritance from the GENO ontology. This simplifies adding inheritance as
#' logical subClassOf axioms supporting curation.
#'
#' NOTE: OMIM phenotypic series on https://omim.org/ include the same data as
#' entries but column are ordered differently.
#'
#' @export
read_omim <- function(file, keep_mim = c("#", "%"), ...) {
    df <- preprocess_omim_dl(file, ...)
    omim_type <- class(df)[1]

    if (omim_type == "omim_search") {
        df <- df %>%
            dplyr::mutate(
                mim_symbol = stringr::str_extract(.data$mim_number, "^[*+#%^]"),
                mim_symbol = tidyr::replace_na(.data$mim_symbol, "none"),
                mim_type = dplyr::case_match(
                    .data$mim_symbol,
                    "*" ~ "gene",
                    "+" ~ "gene, includes phenotype",
                    "#" ~ "phenotype",
                    "%" ~ "phenotype, unknown molecular basis",
                    "^" ~ "deprecated",
                    .default = "phenotype, suspected/overlap"
                ),
                mim_number = stringr::str_remove(.data$mim_number, "^[*+#%^]"),
                omim = paste0("MIM:", .data$mim_number)
            ) %>%
            dplyr::relocate("omim", "mim_symbol", "mim_type", .before = 1)

        if (!is.null(keep_mim)) {
            mim_sym <- c("*", "+", "#", "%", "^", "none")
            mim_mismatch <- keep_mim[!keep_mim %in% mim_sym]
            if (length(mim_mismatch) > 0) {
                rlang::abort(
                    c(
                        '`keep_mim` must be one or more of "*", "+", "#", "%", "^", or "none"',
                        stats::setNames(
                            sandwich_text(mim_mismatch, '"'),
                            rep("x", length(mim_mismatch))
                        )
                    )
                )
            }

            df <- dplyr::filter(df, .data$mim_symbol %in% keep_mim)
        }
    }

    if (omim_type == "omim_PS_titles") {
        df <- df %>%
            dplyr::mutate(
                omim = paste0("MIM:", .data$phenotypic_series_number)
            ) %>%
            dplyr::relocate("omim", .before = 1)
    }

    # ensure output matches ordering of data columns at omim.org for entries
    # (empty cols will be added if missing)
    entry_col_ordered <- c(
        "location", "phenotype", "phenotype_mim_number", "inheritance",
        "phenotype_mapping_key", "gene_locus", "gene_locus_mim_number"
    )
    omim_phenotype <- all(entry_col_ordered %in% names(df))
    if (omim_type == "omim_PS" || omim_phenotype) {
        df <- df %>%
            dplyr::mutate(
                omim = paste0("MIM:", .data$phenotype_mim_number),
                geno_inheritance = dplyr::case_when(
                    .data$inheritance == "AR" ~ "autosomal recessive inheritance",
                    .data$inheritance == "AD" ~ "autosomal dominant inheritance",
                    .data$inheritance == "XLR" ~ "X-linked recessive inheritance",
                    .data$inheritance == "XLD" ~ "X-linked recessive inheritance",
                    .data$inheritance == "XL" ~ "X-linked inheritance",
                    stringr::str_detect(.data$inheritance, stringr::coll("AR")) &
                        stringr::str_detect(.data$inheritance, stringr::coll("AD")) ~ "autosomal inheritance",
                    stringr::str_detect(.data$inheritance, stringr::coll("XLR")) &
                        stringr::str_detect(.data$inheritance, stringr::coll("XLD")) ~ "X-linked inheritance",
                    .default = NA_character_
                )
            )

        df <- append_empty_col(
            df,
            col = c("omim", entry_col_ordered, "geno_inheritance"),
            order = TRUE
        )
    }
    df
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

#' Identify Obsolete (INTERNAL)
#'
#' Identify obsolete terms of the Human Disease Ontology.
#'
#' @param x An object with information about obsolete terms (use [methods()] for
#'     available classes).
#' @param ... Not used; included for extensibility.
#'
#' @keywords internal
identify_obsolete <- function(x, ...) {
    UseMethod("identify_obsolete")
}

#' @export
identify_obsolete.doid_edit <- function(x, ...) {
    match_res <- stringr::str_match(x, "Class.*DOID_([0-9]+).*obsolete")[, 2]
    obs_lui <- stats::na.omit(match_res)
    obs <- paste0("DOID:", obs_lui)
    obs
}


#' Identify Data Missing from an Ontology
#'
#' Identifies data missing from an ontology. Currently only works for mappings,
#' includes both xrefs and skos matches. Requires
#' [ROBOT](https://robot.obolibrary.org/) for comparison.
#'
#' @param onto_path The path to an ontology -edit file, as a string.
#' @param input The path to a TSV/CSV file with data to compare against the data
#'     in the ontology, as a string.
#' @param what What to compare between the two files, as a string.
#' @param report_present Whether to report data present in the ontology
#'      (default) along with data missing, as a boolean.
#'
#' @returns
#' For `report_present = TRUE` (default), a list with two data.frames indicating
#' data in the ontology (`in_onto`) and data missing (`missing`); otherwise,
#' a single data.frame showing the missing data only.
#'
#' @examples
#' \dontrun{
#' onto_missing(
#'     onto_path = "~/Ontologies/HumanDiseaseOntology/src/ontology/doid-edit.owl",
#'     input = "omimps.csv", # e.g. manually copied from https://www.omim.org/phenotypicSeries/PS609060
#'     "OMIM"
#' )
#' }
#' @export
onto_missing <- function(onto_path, input, what = "OMIM",
                         report_present = TRUE) {
    if (!file.exists(onto_path)) rlang::abort("`onto_path` does not exist.")
    if (!file.exists(input)) rlang::abort("`input` does not exist.")
    if (!is_boolean(report_present)) {
        rlang::abort("`report_present` should be a boolean.")
    }

    xref <- c(
        "EFO", "GARD", "ICD10CM", "ICD11", "ICD9CM", "ICDO", "KEGG", "MEDDRA",
        "MESH", "NCI", "OMIM", "ORDO", "SNOMEDCT", "UMLS_CUI"
    )
    if (!what %in% xref) rlang::abort("Only xrefs currently supported.")
    if (!rlang::is_string(what)) rlang::abort("`what` must be a string.")

    if (what == "OMIM") {
        from_input <- read_omim(input)
    } else {
        from_input <- read_delim_auto(input)
    }

    q_out <- tempfile(fileext = ".tsv")
    q <- system.file(
        "sparql", "mapping-all.rq",
        package = "DO.utils",
        mustWork = TRUE
    )
    res <- robot("query", i = onto_path, query = q, q_out)
    from_onto <- readr::read_tsv(
        q_out,
        name_repair = ~ stringr::str_remove(.x, "^\\?"),
        show_col_types = FALSE
    ) %>%
        tidy_sparql()

    if (what == "OMIM") {
        compare_by <- c("mapping" = "omim")
        from_input <- dplyr::select(
            from_input,
            omim, phenotype, location, inheritance,
            dplyr::everything()
        )
    } else {
        compare_by <- "mapping"
    }

    in_onto <- dplyr::inner_join(from_onto, from_input, by = compare_by)
    missing <- dplyr::anti_join(
        from_input,
        from_onto,
        by = invert_nm(compare_by)
    )

    if (report_present) {
        list(in_onto = in_onto, missing = missing)
    } else {
        missing
    }
}

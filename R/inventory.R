#' Assess whether OMIM Data is in DO
#'
#' Assesses whether OMIM identifiers are present in the Human Disease Ontology
#' as mappings (either xrefs or skos mappings). Utilizes [robot()] for
#' comparison.
#'
#' @param onto_path The path to an ontology file, as a string.
#' @param omim_input An `omim_tbl` created by [read_omim()] or the path to a
#' .tsv or .csv file (possibly compressed) that can be read by [read_omim()] and
#' includes OMIM data to compare against the mappings in the ontology.
#'
#' @returns
#' The `omim_input` with 4 columns added:
#' - `exists`: Boolean indicating whether an OMIM ID is present in the DO.
#' - `mapping_type`: The mapping predicate(s) of this OMIM ID to a disease, if
#' present. Multiple predicate(s) will be pipe delimited.
#' - `doid`: The DOID of the disease mapped to this OMIM ID, if present.
#' - `do_label`: The label of the disease mapped to this OMIM ID, if present.
#' - `do_dep`: Boolean indicating whether a disease is deprecated or not, if
#' present.
#'
#' Output will have the class `omim_inventory`, a type of class
#' `mapping_inventory`.
#'
#' @examples
#' \dontrun{
#' # manually copy or download data from https://www.omim.org/phenotypicSeries/PS609060
#' inventory_omim(
#'     onto_path = "~/Ontologies/HumanDiseaseOntology/src/ontology/doid-edit.owl",
#'     omim_input = "omimps.csv",
#' )
#' }
#'
#' @export
inventory_omim <- function(onto_path, omim_input) {
    stopifnot("`onto_path` does not exist." = file.exists(onto_path))

    if ("omim_tbl" %in% class(omim_input)) {
        out <- omim_input
    } else if (file.exists(omim_input)) {
        out <- read_omim(omim_input)
    } else {
        rlang::abort(
            "`omim_input` must be an `omim_tbl` or the path to an existing file."
        )
    }

    # get DO-OMIM mappings
    q_out <- tempfile(fileext = ".tsv")
    q <- system.file(
        "sparql", "mapping-all.rq",
        package = "DO.utils",
        mustWork = TRUE
    )
    res <- robot("query", i = onto_path, query = q, q_out)
    do_mappings <- readr::read_tsv(
        q_out,
        name_repair = ~ stringr::str_remove(.x, "^\\?"),
        show_col_types = FALSE
    ) %>%
        tidy_sparql()

    do_omim <- do_mappings %>%
        dplyr::filter(stringr::str_detect(.data$mapping, "OMIM")) %>%
        dplyr::rename(
            doid = .data$id, do_label = .data$label, do_dep = .data$dep,
            omim = .data$mapping
        ) %>%
        collapse_col(.data$mapping_type, na.rm = TRUE)

    out <- out %>%
        dplyr::left_join(do_omim, by = "omim") %>%
        append_empty_col(
            col = c("exists", "mapping_type", "doid", "do_label", "do_dep")
        ) %>%
        dplyr::mutate(exists = !is.na(.data$doid)) %>%
        dplyr::relocate(
            c(.data$mapping_type, .data$exists),
            .before = .data$doid
        )

    class(out) <- c("omim_inventory", "mapping_inventory", class(out))

    out
}


#' Report Inventory Results
#'
#' Reports on results of an `inventory_*()` run.
#'
#' @param inventory_df A `*_inventory` data.frame created by one of DO.utils'
#'     `inventory_*()` functions.
#' @param verbose Whether to report results on screen (default: `TRUE`).
#'
#' @returns A list of data.frames including statistical `counts` and results for
#'     review, invisibly.
#'
#' @export
inventory_report <- function(inventory_df, verbose = TRUE) {
    UseMethod("inventory_report")
}

#' @rdname inventory_report
#'
#' @returns Specific `omim_inventory` method data.frames:
#' - `doid_deprecated`,
#' - `omim_to_many`: Results where an OMIM ID corresponds to multiple
#' DOIDs, excluding skos broad/narrow/related matches.
#' - `doid_to_many`): Results where a DOID corresponds to multiple
#' OMIM IDs, excluding skos broad/narrow/related matches.
#'
#' @export
inventory_report.omim_inventory <- function(inventory_df, verbose = TRUE) {
    dep <- dplyr::filter(inventory_df, isTRUE(.data$do_dep))
    omim_to_many <- maps_to_many(inventory_df, .data$omim, .data$doid)
    doid_to_many <- maps_to_many(inventory_df, .data$omim, .data$doid)
    stats <- tibble::tribble(
        ~ "report", ~ "n",
        "omim_total", dplyr::n_distinct(inventory_df$omim, na.rm = TRUE),
        "omim_absent", dplyr::n_distinct(inventory_df$omim[is.na(inventory_df$doid)]),
        "omim_present", dplyr::n_distinct(inventory_df$omim[!is.na(inventory_df$doid)]),
        "omim_to_many", dplyr::n_distinct(omim_to_many$omim),
        "doid_total", dplyr::n_distinct(inventory_df$doid, na.rm = TRUE),
        "doid_deprecated", dplyr::n_distinct(dep$doid),
        "doid_to_many", dplyr::n_distinct(doid_to_many$doid)
    )
    class(stats) <- c("oirs", class(stats))

    if (verbose) {
        print(stats)
    }

    out <- list(
        stats = stats,
        doid_deprecated = dep,
        omim_to_many = omim_to_many,
        doid_to_many = doid_to_many
    )

    invisible(out)
}


# inventory_report() helpers ----------------------------------------------

maps_to_many <- function(.df, .col1, .col2) {
    many_lgl <- .df %>%
        dplyr::group_by({{ .col1 }}) %>%
        dplyr::mutate(
            many = sum(
                stringr::str_detect(.data$mapping_type, "skos:(exact|close)")
            ) > 1
        ) %>%
        dplyr::ungroup()

    out <- dplyr::filter(many_lgl, .data$many) %>%
        dplyr::select(-"many")

    out
}

#' Print OMIM inventory report statistics
#'
#' Prints OMIM inventory report statistics.
#'
#' @export
print.oirs <- function(x, ...) {
    oirs_new <- x %>%
        dplyr::mutate(
            id_type = stringr::str_remove(.data$report, "_.+"),
            stat = stringr::str_remove(.data$report, "[^_]+_"),
            report = NULL
        )
    oirs_split <- split(oirs_new, oirs_new$id_type)
    omim <- array(
        oirs_split$omim$n,
        dim = c(1, 4),
        list("OMIM:", oirs_split$omim$stat)
    )
    doid <- array(
        oirs_split$doid$n,
        dim = c(1, 3),
        list("DOID:", oirs_split$doid$stat)
    )

    print(omim, quote = FALSE)
    print(doid, quote = FALSE)
}

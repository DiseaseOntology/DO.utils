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
#' @inheritParams multimaps
#'
#' @returns
#' The `omim_input` with 5 additional columns:
#' - `exists`: Logical indicating whether an OMIM ID is present in the DO.
#' - `mapping_type`: The mapping predicate(s) of this OMIM ID to a disease, if
#' present. Multiple predicate(s) between the same OMIM and DOID will be pipe
#' delimited.
#' - `doid`: The DOID of the disease mapped to this OMIM ID, if present.
#' - `do_label`: The label of the disease mapped to this OMIM ID, if present.
#' - `do_dep`: Logical indicating whether a disease is deprecated or not, if
#' present.
#' - `multimaps`: The direction in which an OMIM or DO term maps to multiple
#' terms in the other resource, as "omim_to_doid", "doid_to_omim", "both_ways"
#' or `NA`.
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
inventory_omim <- function(onto_path, omim_input, include_hasDbXref = TRUE) {
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

    # identify terms that multimap
    omim_mm <- multimaps(
        out$omim,
        out$mapping_type,
        out$doid,
        include_hasDbXref = include_hasDbXref
    )
    doid_mm <- multimaps(
        out$doid,
        out$mapping_type,
        out$omim,
        include_hasDbXref = include_hasDbXref
    )
    out <- dplyr::mutate(
        out,
        multimaps = dplyr::case_when(
            omim_mm & doid_mm ~ "both_ways",
            omim_mm ~ "omim_to_doid",
            doid_mm ~ "doid_to_omim",
            TRUE ~ NA_character_
        )
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
#' @param ... Additional arguments for methods. Currently unused.
#'
#' @returns A list of data.frames including statistical counts and results for
#'     review, invisibly.
#'
#' @export
inventory_report <- function(inventory_df, verbose = TRUE, ...) {
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
inventory_report.omim_inventory <- function(inventory_df, verbose = TRUE, ...) {
    dep <- dplyr::filter(inventory_df, isTRUE(.data$do_dep))
    omim_to_many <- dplyr::filter(
        inventory_df,
        .data$multimaps %in% c("omim_to_doid", "both_ways")
    )
    doid_to_many <- dplyr::filter(
        inventory_df,
        .data$multimaps %in% c("doid_to_omim", "both_ways")
    )
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

#' Identify One-to-Multiple Mappings
#'
#' Identifies values in `x` that map to multiple values in `y` for
#' `skos:exactMatch` or `skos:closeMatch` (and optionally also
#' `oboInOwl:hasDbXref`) mapping predicates.
#'
#' @param x Vector with `subject` of mappings (i.e. those being tested; the
#'     "one" in the "one-to-multiple" test).
#' @param pred Vector with predicate(s) of mappings. Predicate(s) should
#'     be formatted as CURIEs but can include multiple delimited predicates.
#' @param y Vector with `object` of mappings (i.e. those being counted; the
#'     "multiple" in the "one-to-multiple" test).
#' @param include_hasDbXref Whether `oboInOwl:hasDbXrefs` should be included in
#'     tests for "one-to-multiple" mappings, as a boolean (default: `TRUE`).
#'     `skos:exactMatch` & `skos:closeMatch` mappings are always included.
#'
#' @returns A logical vector specifying the positions in `x` that map to
#' multiple values in `y`.
#'
#' @keywords internal
multimaps <- function(x, pred, y, include_hasDbXref = TRUE) {
    stopifnot(
        "`x`, `pred`, & `y` must be the same length" =
            dplyr::n_distinct(c(length(x), length(pred), length(y))) == 1
    )
    if (include_hasDbXref) {
        mapping_pattern <- "skos:(exact|close)|hasDbXref"
    } else {
        mapping_pattern <- "skos:(exact|close)"
    }

    p_incl <- stringr::str_detect(pred, mapping_pattern) & !is.na(pred)
    pi_split <- split(p_incl, x)
    y_split <- split(y, x)
    multimaps <- vapply(
        1:length(y_split),
        function(i) {
            y_in <- y_split[[i]][pi_split[[i]]]
            dplyr::n_distinct(y_in, na.rm = TRUE) > 1
        },
        FUN.VALUE = FALSE
    )
    out <- x %in% names(y_split)[multimaps]
    out
}

#' Print OMIM inventory report statistics
#'
#' Prints OMIM inventory report statistics.
#'
#' @param x An object of class `oirs`.
#' @inheritDotParams base::print.default -quote
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

    print(omim, quote = FALSE, ...)
    print(doid, quote = FALSE, ...)
}

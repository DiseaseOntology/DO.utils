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
#' NOTE: If an `omim_tbl` is provided, `keep_mim` will be ignored.
#' @inheritParams read_omim
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
inventory_mapping <- function(onto_path, mapping,
                           include_pred = c("skos:exactMatch", "skos:closeMatch", "oboInOwl:hasDbXref"),
                           when_pred_NA = "error") {
    stopifnot("`onto_path` does not exist." = file.exists(onto_path))

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
        tidy_sparql() %>%
        dplyr::rename(
            subject_id = .data$id, subject_label = .data$label, subject_dep = .data$dep, # update SPARQL, remove this
            object_id = .data$mapping, predicate_id = .data$mapping_type
        ) %>%
        collapse_col(.data$predicate_id, na.rm = TRUE)

    if (!is.data.frame(mapping)) {
        out <- tibble::tibble(object_id = mapping) %>%
            tidy_sparql()
    } else if(!"id" %in% names(mapping) && !"object_id" %in% names(mapping)) {
        rlang::abort(
            "`mapping` must have an 'id' or 'object_id' column"
        )
    } else {
        out <- mapping
    }

    if ("label" %in% names(mapping)) {
        out <- dplyr::rename(out, object_label = "label")
    }

    if ("id" %in% names(mapping)) {
        out <- dplyr::rename(out, object_id = "id")
    }

    out <- out %>%
        dplyr::left_join(do_mappings, by = "object_id") %>%
        append_empty_col(
            col = c("exists", "predicate_id", "subject_id", "subject_label", "subject_dep")
        ) %>%
        dplyr::mutate(exists = !is.na(.data$subject_id)) %>%
        dplyr::relocate(
            c(.data$predicate_id, .data$exists),
            .before = .data$subject_id
        )

    # identify terms that multimap
    object_mm <- multimaps(
        out$object_id,
        out$predicate_id,
        out$subject_id,
        when_pred_NA = when_pred_NA
    )
    subject_mm <- multimaps(
        out$subject_id,
        out$predicate_id,
        out$object_id,
        when_pred_NA = when_pred_NA
    )
    out <- dplyr::mutate(
        out,
        multimaps = dplyr::case_when(
            object_mm & subject_mm ~ "both_ways",
            object_mm ~ "object_to_subject",
            subject_mm ~ "subject_to_object",
            TRUE ~ NA_character_
        )
    )

    class(out) <- c("mapping_inventory", class(out))

    out
}


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
#' NOTE: If an `omim_tbl` is provided, `keep_mim` will be ignored.
#' @inheritParams read_omim
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
inventory_omim <- function(onto_path, omim_input, keep_mim = c("#", "%"),
                           include_pred = c("skos:exactMatch", "skos:closeMatch", "oboInOwl:hasDbXref"),
                           when_pred_NA = "error") {
    stopifnot("`onto_path` does not exist." = file.exists(onto_path))

    if ("omim_tbl" %in% class(omim_input)) {
        out <- omim_input
    } else if (file.exists(omim_input)) {
        out <- read_omim(omim_input, keep_mim = keep_mim)
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
        dplyr::filter(stringr::str_detect(.data$mapping, "O?MIM")) %>%
        dplyr::rename(
            doid = .data$id, do_label = .data$label, do_dep = .data$dep,
            omim = .data$mapping
        ) %>%
        collapse_col(.data$mapping_type, na.rm = TRUE)

    # convert OMIM: prefix to MIM: (preferred) with warning, if needed
    if (any(stringr::str_detect(do_omim$omim, "OMIM"))) {
        rlang::warn("`onto_path` file uses an unpreferred OMIM prefix. Converting to 'MIM'...")
        do_omim <- do_omim %>%
            dplyr::mutate(omim = stringr::str_replace(omim, "OMIM:", "MIM:"))
    }

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
        when_pred_NA = when_pred_NA
    )
    doid_mm <- multimaps(
        out$doid,
        out$mapping_type,
        out$omim,
        when_pred_NA = when_pred_NA
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

#' Assess whether Data is in DO
#'
#' Assesses whether identifiers are present in the Human Disease Ontology
#' as mappings (either xrefs or skos mappings). Utilizes [robot()] for
#' comparison.
#'
#' @param onto_path The path to an ontology file, as a string.
#' @param ordo_input An `omim_tbl` created by [read_omim()] or the path to a
#' .tsv or .csv file (possibly compressed) that can be read by [read_omim()] and
#' includes OMIM data to compare against the mappings in the ontology.
#' @param ordo_type The type to expect for `ordo_input`, as a string. One of:
#'
#' * "official": the official OWL release of ORDO
#' * "converted": OWL with various conversions to make it like DO. See
#' [convert_ordo()] for details.
#'
#' @inheritParams read_omim
#' @inheritParams multimaps
#'
#' @returns
#' A tibble of SSSOM mappings. **NOTE** that DO disease terms will appear in the
#' _object_ fields of this output, instead of the more common _subject_ fields.
#' - `subject_id`: The ID of each ORDO term.
#' - `subject_label`: The label of each ORDO term.
#' - `predicate_id`: The mapping predicate(s) of this ORDO ID to a DO disease,
#' if present. Multiple predicate(s) between the same OMIM and DOID will be pipe
#' delimited.
#' - `object_id`: The DOID of the disease mapped to this ORDO ID, if present.
#' - `object_label`: The label of the disease mapped to this ORDO ID, if present.
#' - `object_dep`: Logical indicating whether a DO disease is deprecated or not,
#' if present.
#' - `multimaps`: The direction in which an ORDO or DO term maps to multiple
#' terms in the other resource, as "subject_to_object", "object_to_subject",
#' "both_ways" or `NA`.
#'
#' Output will have the class `ordo_inventory`, a type of class
#' `mapping_inventory`.
#'
#' @examples
#' \dontrun{
#' # manually copy or download data from https://www.omim.org/phenotypicSeries/PS609060
#' inventory_ordo(
#'     onto_path = "~/Ontologies/HumanDiseaseOntology/src/ontology/doid-edit.owl",
#'     ordo_input = "ORDO_en_4.5.owl",
#'     ordo_type = "official"
#' )
#' }
#'
#' @export
inventory_ordo <- function(onto_path, ordo_input, ordo_type = NULL,
                           include_pred = c("skos:exactMatch", "skos:closeMatch", "oboInOwl:hasDbXref"),
                           when_pred_NA = "error") {
    ordo_type <- match.arg(ordo_type, choices = c("official", "converted"))
    stopifnot(
        "`onto_path` does not exist." = file.exists(onto_path),
        "`ordo_input` does not exist." = file.exists(ordo_input),
    )

    if ("ordo_type" == official) {
        # convert mappings using tempfile
        # extract mappings
        out <- omim_input
    } else if (file.exists(omim_input)) {
        out <- read_omim(omim_input, keep_mim = keep_mim)
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
        dplyr::filter(stringr::str_detect(.data$mapping, "O?MIM")) %>%
        dplyr::rename(
            doid = .data$id, do_label = .data$label, do_dep = .data$dep,
            omim = .data$mapping
        ) %>%
        collapse_col(.data$mapping_type, na.rm = TRUE)

    # convert OMIM: prefix to MIM: (preferred) with warning, if needed
    if (any(stringr::str_detect(do_omim$omim, "OMIM"))) {
        rlang::warn("`onto_path` file uses an unpreferred OMIM prefix. Converting to 'MIM'...")
        do_omim <- do_omim %>%
            dplyr::mutate(omim = stringr::str_replace(omim, "OMIM:", "MIM:"))
    }

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
        when_pred_NA = when_pred_NA
    )
    doid_mm <- multimaps(
        out$doid,
        out$mapping_type,
        out$omim,
        when_pred_NA = when_pred_NA
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

# inventory_omim() helpers ----------------------------------------------

#' Identify One-to-Multiple Mappings
#'
#' Identifies values in `x` that map to multiple values in `y` for specified
#' mapping predicates.
#'
#' @param x Vector with `subject` of mappings (i.e. those being tested; the
#'     "one" in the "one-to-multiple" test).
#' @param pred Vector with predicate(s) of mappings. Predicate(s) should
#'     be formatted as CURIEs but can include multiple delimited predicates.
#' @param y Vector with `object` of mappings (i.e. those being counted; the
#'     "multiple" in the "one-to-multiple" test).
#' @param include_pred The predicates to include when testing for one-to-multiple
#'     mappings, as a character vector (default: `skos:exactMatch`,
#'     `skos:closeMatch`, and `oboInOwl:hasDbXref`). All other predicates are
#'     ignored.
#' @param when_pred_NA What to do when missing predicates are detected, as a
#'     string; one of "error" (default), "warn", or NULL (do nothing). `NA`
#'     predicates are _always_ ignored when no mapping exists (i.e. one or both
#'     corresponding values of `x` or `y` is/are also `NA`).
#'
#' @returns A logical vector specifying the positions in `x` that map to
#' multiple values in `y`. Incomplete mappings, where values of `x`, `y`, or
#' both are `NA`, are ignored and return `FALSE`.
#'
#' @keywords internal
multimaps <- function(x, pred, y,
                      include_pred = c("skos:exactMatch", "skos:closeMatch", "oboInOwl:hasDbXref"),
                      when_pred_NA = "error") {
    stopifnot(
        "`x`, `pred`, & `y` must be the same length" =
            dplyr::n_distinct(c(length(x), length(pred), length(y))) == 1
    )

    if (all(is.na(x)) || all(is.na(y))) {
        out <- rep(FALSE, length(x))
        return(out)
    }

    p_missing <- is.na(pred) & !is.na(x) & !is.na(y)
    if (any(p_missing)) {
        rlang::abort(
            c(
                "Predicates must not be missing from mappings",
                x = paste0("`pred` = `NA` [", to_range(which(p_missing)), "]")
            )
        )
    }

    include_pattern <- unique_to_string(include_pred, delim = "|")
    p_incl <- stringr::str_detect(pred, include_pattern)
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

#' Assess whether OMIM Data is in DO
#'
#' Assesses whether OMIM identifiers are present in the Human Disease Ontology
#' as mappings (either xrefs or skos mappings). Utilizes [robot()] for
#' comparison.
#'
#' @param onto_path The path to an ontology file, as a string.
#' @param omim_input An `omim_tbl` created by [read_omim()] or the path to a
#'   .tsv or .csv file (possibly compressed) that can be read by [read_omim()]
#'    and includes OMIM data to compare against the mappings in the ontology.
#'
#'   NOTE: If an `omim_tbl` is provided, `keep_mim` will be ignored.
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
#' # execute within the HumanDiseaseOntology repository and download data from
#' # https://www.omim.org/phenotypicSeries/PS609060 to omimps.tsv
#' inventory_omim(
#'   onto_path = "src/ontology/doid-edit.owl",
#'   omim_input = "omimps.tsv",
#' )
#' }
#'
#' @export
inventory_omim <- function(
  onto_path,
  omim_input,
  keep_mim = c("#", "%"),
  include_pred = c("skos:exactMatch", "skos:closeMatch", "oboInOwl:hasDbXref"),
  when_pred_NA = "error"
) {
  if (!file.exists(onto_path)) {
    rlang::abort("`onto_path` does not exist.")
  }

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
  q <- system.file(
    "sparql",
    "mapping-all.rq",
    package = "DO.utils",
    mustWork = TRUE
  )
  do_mappings <- robot_query(onto_path, q, tidy_what = "everything")

  do_omim <- do_mappings |>
    dplyr::filter(stringr::str_detect(.data$mapping, "O?MIM")) |>
    dplyr::rename(
      doid = .data$id,
      do_label = .data$label,
      do_dep = .data$dep,
      omim = .data$mapping
    ) |>
    collapse_col(.data$mapping_type, na.rm = TRUE)

  # convert OMIM prefix to MIM (preferred) with warning, if needed
  do_omim <- do_omim |>
    dplyr::mutate(omim = prefer_mim(.data$omim, warn_arg_nm = "onto_path"))

  out <- out |>
    dplyr::left_join(do_omim, by = "omim") |>
    append_empty_col(
      col = c("exists", "mapping_type", "doid", "do_label", "do_dep")
    ) |>
    dplyr::mutate(exists = !is.na(.data$doid)) |>
    dplyr::relocate("mapping_type", "exists", .before = "doid")

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


#' Assess whether OMIM susceptibilities are in the DO
#'
#' Assesses whether OMIM entries are present in the Human Disease Ontology as
#' susceptibilities (in the `omim_susc_import.owl` file). Utilizes [robot()] for
#' comparison.
#'
#' @param susc_path The path to the `omim_susc_import.owl` file, as a string.
#' @param omim_input An `omim_tbl` created by [read_omim()] or the path to a
#'   .tsv or .csv file (possibly compressed) that can be read by [read_omim()] and
#'   includes OMIM data to compare against the susceptibility classes in the
#'   ontology.
#'
#'   NOTE: If an `omim_tbl` is provided, `keep_mim` will be ignored.
#' @param do_path The path to a Human Disease Ontology file, as a string, or
#'   `NULL` (default). If provided, additional information about the DOIDs
#'   (labels, deprecated status) that are related to the susceptibilities will
#'   be included in the output.
#' @inheritParams read_omim
#'
#' @returns
#' The `omim_input` with 4 additional columns:
#' - `exists`: Logical indicating whether an OMIM ID is present in the DO as a
#' susceptibility.
#' - `susc_label`: The label of the susceptibility.
#' - `susc_dep`: Logical indicating whether the susceptibility is deprecated or
#' not.
#' - `related_doid`: All disease(s) related to a given OMIM susceptibility
#' (delimited by " | "). If `do_path` is provided, the data will be formatted as
#' "label (DOID; deprecated)" for each related disease; otherwise, only the
#' DOID(s) will be included.
#'
#' Output will have the class `omim_susc_inventory`.
#'
#' @examples
#' \dontrun{
#' # execute within the HumanDiseaseOntology repository and download data from
#' # https://www.omim.org/phenotypicSeries/PS145600 to omimps.tsv
#' inventory_omim_susc(
#'   susc_path = "src/ontology/omim_susc_import.owl",
#'   omim_input = "omimps.tsv",
#'   do_path = "src/ontology/doid-edit.owl"
#' )
#' }
#'
#' @export
inventory_omim_susc <- function(
  susc_path,
  omim_input,
  do_path = NULL,
  keep_mim = c("#", "%")
) {
  if (!file.exists(susc_path)) {
    rlang::abort("`susc_path` does not exist.")
  }
  if (!is.null(do_path) && !file.exists(do_path)) {
    rlang::abort("`do_path` does not exist.")
  }

  if ("omim_tbl" %in% class(omim_input)) {
    out <- omim_input
  } else if (file.exists(omim_input)) {
    out <- read_omim(omim_input, keep_mim = keep_mim)
  } else {
    rlang::abort(
      "`omim_input` must be an `omim_tbl` or the path to an existing file."
    )
  }

  # get OMIM susceptibilities
  q_susc <- system.file(
    "sparql",
    "omim-susc.rq",
    package = "DO.utils",
    mustWork = TRUE
  )
  omim_susc <- robot_query(susc_path, q_susc, tidy_what = "everything")

  omim_info <- omim_susc |>
    dplyr::rename(omim = "iri", susc_label = "label", susc_dep = "dep") |>
    # convert OMIM prefix to MIM (preferred) with warning, if needed, and
    # drop "obo:" prefix
    # -> to_curie(), correctly, does not treat "obo:MIM_" = "MIM:"
    dplyr::mutate(
      omim = stringr::str_replace(
        prefer_mim(.data$omim, warn_arg_nm = "susc_path"),
        "obo:([^_]+)_",
        "\\1:"
      )
    )

  # optionally, add more DOID info (label, deprecated)
  if (is.null(do_path)) {
    omim_info <- dplyr::rename(omim_info, related_doid = "do_iri")
  } else {
    q_do <- system.file(
      "sparql",
      "class-label.rq",
      package = "DO.utils",
      mustWork = TRUE
    )
    do_info <- robot_query(do_path, q_do, tidy_what = "everything")
    do_join <- do_info |>
      dplyr::mutate(
        dep = dplyr::if_else(.data$dep, "; deprecated", ""),
        related_doid = paste0(.data$label, " (", .data$iri, .data$dep, ")")
      ) |>
      dplyr::select(do_iri = "iri", "related_doid")

    omim_info <- omim_info |>
      dplyr::left_join(do_join, by = "do_iri") |>
      dplyr::select(-"do_iri")
  }

  omim_info <- collapse_col(omim_info, .data$related_doid, delim = " | ")

  out <- out |>
    dplyr::left_join(omim_info, by = "omim") |>
    append_empty_col(
      col = c("exists", "susc_label", "susc_dep", "related_doid")
    ) |>
    dplyr::mutate(exists = !is.na(.data$susc_label)) |>
    dplyr::relocate(
      "exists",
      "susc_label",
      "susc_dep",
      .before = "related_doid"
    )

  class(out) <- c("omim_susc_inventory", class(out))

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
multimaps <- function(
  x,
  pred,
  y,
  include_pred = c("skos:exactMatch", "skos:closeMatch", "oboInOwl:hasDbXref"),
  when_pred_NA = "error"
) {
  if (dplyr::n_distinct(c(length(x), length(pred), length(y))) != 1) {
    rlang::abort("`x`, `pred`, & `y` must be the same length.")
  }

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
    seq_along(y_split),
    function(i) {
      y_in <- y_split[[i]][pi_split[[i]]]
      dplyr::n_distinct(y_in, na.rm = TRUE) > 1
    },
    FUN.VALUE = FALSE
  )
  out <- x %in% names(y_split)[multimaps]
  out
}

# convert OMIM prefix to MIM (preferred) with warning, if needed
prefer_mim <- function(x, warn_arg_nm = NULL) {
  if (!any(stringr::str_detect(x, "OMIM"))) {
    return(x)
  }
  rlang::warn(
    paste0(
      sandwich_text(warn_arg_nm, "`"),
      " includes the unpreferred 'OMIM' prefix. Converting to 'MIM'..."
    )
  )
  stringr::str_replace(x, "OMIM", "MIM")
}

#' Create a Curation Template
#'
#' Create a curation template in a Google Sheet, optionally including data.
#'
#' @inheritParams googlesheets4::range_write
#' @param .data Data to add to the curation sheet. If `NULL` (default), an empty
#' curation sheet will be created.
#' @param sheet (OPTIONAL) The sheet name, as a string. If `NULL` (default), the
#' sheet name will default to "curation-" with today's date appended (formatted
#' as "%Y%m%d"; see [format.Date()]).
#' @param ... Additional arguments passed to methods.
#'
#' @returns The Google Sheet info (`ss`), as a [googlesheets4::sheets_id].
#'
#' @section Formatting Limitations:
#' Formatting to make data more visually distinct is not currently supported due
#' to limitations of the Google Sheets API and the `googlesheets4` package:
#' * Google Sheets API does not support assigning colors to data validation.
#' * `googlesheets4` does not support any formatting.
#'
#' An alternative approach to support some formatting could be to create a
#' functional template with the desired formatting and copy that template with
#' [googlesheets4::sheet_copy()]. The `data_type` could still be populated by
#' this function (only needed to support types not in
#' `.curation_opts$data_type`).
#'
#' @export
curation_template <- function(.data = NULL, ss = NULL, sheet = NULL, ...) {
    UseMethod("curation_template", .data)
}

#' @param nrow The number of rows to create in the curation template when
#' `.data = NULL` (default: `50`).
#'
#' @export
#' @rdname curation_template
curation_template.NULL <- function(.data = NULL, ss = NULL, sheet = NULL, ...,
                                   nrow = 50) {
  val <- rep(NA, nrow)

  # inspired by https://stackoverflow.com/a/60495352/6938922
  cur_df <- tibble::as_tibble(rlang::rep_named(curation_cols, list(val)))

  class(cur_df) <- c("curation_template", class(cur_df))
  if (is.null(sheet)) sheet <- paste0("curation-", format(Sys.Date(), "%Y%m%d"))
  gs_info <- googlesheets4::write_sheet(cur_df, ss, sheet)

  if (is.null(ss)) ss <- gs_info
  set_curation_validation(cur_df, ss, sheet)

  invisible(gs_info)
}

#' @param id_max The maximum number of unique classes to include (default: `20`).
#' @param n_id_sep The number of blank rows to insert between each `id` group
#' (default: `2`).
#' @param debug Controls debug output. `FALSE` (default) writes to Google Sheets
#' normally. One or more of:
#' * `"output"`: returns the final data frame visibly instead of writing to
#'   Google Sheets.
#' * `"types"`: returns a list with `$matched` (named character vector where
#'   names are the original predicate strings and values are the resolved
#'   `data_type` labels, as mapped by `.sparql_dt_motif`) and `$unmatched`
#'   (character vector of predicates not in `.sparql_dt_motif`, used as-is).
#'   When combined with `"steps"`, the list is added as `$types` in that output.
#'   Combine with `"output"` to also return the final data frame.
#' * `"steps"`: returns a named list of snapshots at each major pipeline step
#'   (`filtered`, `pivoted`, `typed`, `output`); implies `"output"`. If `"types"`
#'   is also requested, includes `$types` in the returned list.
#'
#' @export
curation_template.obo_data <- function(.data, ss = NULL, sheet = NULL, ...,
                                       id_max = 20, n_id_sep = 2L,
                                       debug = FALSE) {
    if (!isFALSE(debug)) {
        debug <- match.arg(
            debug,
            choices = c("output", "types", "steps"),
            several.ok = TRUE
        )
    }
    if (!is.numeric(id_max) || length(id_max) != 1L || id_max < 1L) {
        rlang::abort("`id_max` must be a single positive integer.")
    }
    all_ids <- unique(.data$id)
    incl_ids <- utils::head(all_ids, id_max)
    excl_ids <- all_ids[!all_ids %in% incl_ids]
    if (length(excl_ids) > 0) {
        max_show <- 10L
        if (length(excl_ids) <= max_show) {
            excl_txt <- paste(excl_ids, collapse = ", ")
        } else {
            excl_txt <- paste0(
                paste(utils::head(excl_ids, max_show), collapse = ", "),
                ", ... and ", length(excl_ids) - max_show, " more"
            )
        }
        rlang::inform(
            paste0(
                length(incl_ids), " of ", length(all_ids),
                " unique IDs included (id_max = ", id_max, ").",
                "\nExcluded: ", excl_txt
            )
        )
    }

    # Step 1: filter & reshape to long format with resolved predicate strings
    step_filtered <- .data |>
        dplyr::filter(.data$id %in% incl_ids)

    # Step 2: pivot to long format
    step_pivoted <- step_filtered |>
        # need smarter indexing... I think, not currently used (see below)
        dplyr::mutate(
            index = dplyr::dense_rank(paste0(.data$predicate, .data$value)),
            .by = "id",
            .before = "id"
        ) |>
        # convert predicate & axiom_predicate to patterns in .sparql_dt_motif
        dplyr::mutate(
            predicate = dplyr::if_else(
                !is.na(.data$axiom_predicate) & .data$axiom_predicate == "oboInOwl:hasSynonymType",
                paste0(.data$predicate, "-", .data$axiom_value),
                .data$predicate
            ),
            axiom_predicate = dplyr::if_else(
                !is.na(.data$axiom_predicate) & .data$axiom_predicate != "oboInOwl:hasSynonymType",
                paste0(.data$predicate, "-", .data$axiom_predicate),
                NA_character_
            ),
            # removes axiom value where predicate is updated (redundant)
            axiom_value = dplyr::if_else(
                is.na(.data$axiom_predicate),
                NA_character_,
                .data$axiom_value
            )
        ) |>
        tidyr::pivot_longer(
            cols = -c("index", "id"),
            names_to = ".value",
            names_prefix = "^axiom_",
            values_drop_na = TRUE
        ) |>
        dplyr::arrange(
            .data$id,
            .data$index,
            stringr::str_length(.data$predicate)
        ) |>
        dplyr::rename(data_type = "predicate", "curation_notes" = "extra") |>
        # for now, just remove index --> need to use for sorting at some point
        dplyr::select(-"index") |>
        unique()

    # Step 3: resolve data_type values via .sparql_dt_motif
    step_typed <- step_pivoted |>
        # collapse_col(value) |> # does nothing... probably don't want to collapse
        dplyr::mutate(
            data_type = dplyr::coalesce(
                .sparql_dt_motif[.data$data_type],
                .data$data_type
            )
        )

    # Step 4: sort, finalise, and add id separators
    cur_df <- step_typed |>
        sort_by_curation_dt() |>
        dplyr::mutate(
            id = dplyr::if_else(duplicated(.data$id), NA_character_, .data$id),
            # set default action for existing data
            action = "retain"
        ) |>
        append_empty_col(curation_cols, order = TRUE) |>
        add_id_sep(n = n_id_sep)

    class(cur_df) <- c("curation_template", class(cur_df))

    # debug paths: never write to Google Sheets
    if (!isFALSE(debug)) {
        types_info <- NULL
        if ("types" %in% debug) {
            raw_types <- unique(step_pivoted$data_type)
            matched <- raw_types[raw_types %in% names(.sparql_dt_motif)]
            unmatched <- raw_types[!raw_types %in% names(.sparql_dt_motif)]
            # $matched: named vector of raw predicate -> resolved data_type
            # $unmatched: raw predicates passed through as-is
            types_info <- list(
                matched   = .sparql_dt_motif[matched],
                unmatched = unmatched
            )
        }
        if ("steps" %in% debug) {
            out <- list(
                filtered = step_filtered,
                pivoted  = step_pivoted,
                typed    = step_typed,
                output   = cur_df
            )
            if (!is.null(types_info)) out$types <- types_info
            return(out)
        }
        if ("types" %in% debug) return(types_info)
        return(cur_df)
    }

    if (is.null(sheet)) sheet <- paste0("curation-", format(Sys.Date(), "%Y%m%d"))
    gs_info <- googlesheets4::write_sheet(cur_df, ss, sheet)

    if (is.null(ss)) ss <- gs_info
    set_curation_validation(cur_df, ss, sheet)

    invisible(gs_info)
}


# helpers --------------------------------------------------------------------

# Insert n blank rows between each id group (identified by non-NA id values).
add_id_sep <- function(.data, n = 2L) {
    grp_id <- cumsum(!is.na(.data$id))
    groups <- split(.data, grp_id)
    blanks <- .data[rep(NA_integer_, n), ]
    purrr::reduce(groups[-1], ~ dplyr::bind_rows(.x, blanks, .y), .init = groups[[1]])
}

# Sort data_type values within each id group per .curation_opts ordering.
# data_type values not found in .curation_opts are placed at the end.
# The existing order of id groups is preserved (not sorted alphabetically).
sort_by_curation_dt <- function(.data) {
    dt_order <- .curation_opts$data_type
    .data |>
        dplyr::mutate(
            .grp = dplyr::consecutive_id(.data$id),
            .dt_rank = match(.data$data_type, dt_order, nomatch = length(dt_order) + 1L)
        ) |>
        dplyr::arrange(.data$.grp, .data$.dt_rank) |>
        dplyr::select(-c(".grp", ".dt_rank"))
}

### define expected columns for curation template (in order) ###

# full set of curations columns
curation_cols <- c(
  "id", "data_type", "value", "action", "curation_notes", "links",
  "action_notes"
)

#' Curation Action
#'
#' Values used to establish `action` data validation in Google Sheets
#' [curation templates][curation_template()].
#'
#' * `retain`: data already in ontology that should be kept; this is the default
#' `action` for existing data when creating a [curation_template()]
#'
#' * `add`: new data that should be added
#'
#' * `remove`: existing ontology data that should be removed
#'
#' * `exclude`: data relevant to the ontology that should be actively excluded
#' (e.g. an incorrect mapping) -- details should be included in `action_notes`
#'
#' * `ignore`: data not for active inclusion or exclusion that should be ignored
#' (e.g. dubious synonyms, incomplete curation data)
#'
#' * `restore`: data that was removed from the ontology and should be added back
#'
#' @keywords internal
curation_action <- c("retain", "add", "remove", "exclude", "ignore", "restore")


# Set Data Validation for Curation Templates
set_curation_validation <- function(cur_df, ss, sheet) {
    # add data_type validation
    dt_range <- spreadsheet_range(cur_df, "data_type")
    range_add_dropdown(ss, sheet, dt_range, values = .curation_opts$data_type)

    # add action validation
    action_range <- spreadsheet_range(cur_df, "action")
    range_add_dropdown(ss, sheet, action_range, values = curation_action)

    # freeze first two columns
    googlesheets4::with_gs4_quiet(
        googlesheets4:::sheet_freeze(ss, sheet = sheet, ncol = 2)
    )
}

#' Calculate a Spreadsheet Range
#'
#' Calculate a range for a spreadsheet program (Google Sheets or Excel).
#'
#' @inheritParams curation_template
#' @param .data A tibble.
#' @param .col The column to use for the range, as a string.
#' @param rows (OPTIONAL) The rows to use for the range, either as a continous
#' integer vector or as a string (i.e. "1:10"). If `NULL` (default), the entire
#' column will be used.
#' @param n_header The number of header rows to skip (default: `1`).
#'
#' @keywords internal
spreadsheet_range <- function(.data, .col, sheet = NULL, rows = NULL,
                                 n_header = 1) {
  col_letter <- colnum_to_ss_letter(which(names(.data) == .col))
  if (length(col_letter) != 1) {
    rlang::abort("Exactly one column must be specified in `.col`")
  }

  if (is.null(rows)) {
    row_ends <- c(1, nrow(.data)) + n_header
  } else if (is.numeric(rows)) {
    # check one continuous range
    collapsed_range <- to_range(rows, sep = c(",", ":"))
    if (stringr::str_count(collapsed_range, "[,:]") > 1) {
      rlang::abort(
        c("`rows` must be one continuous range", x = collapsed_range)
      )
    }
    row_ends <- c(rows[1], utils::tail(rows, 1)) + n_header
  } else {
    row_ends <- as.integer(stringr::str_split(row_ends, ":")[[1]]) + n_header
  }

  range <- paste0(col_letter, row_ends, collapse = ":")
  if (!is.null(sheet)) {
    range <- paste0(sheet, "!", range)
  }
  range
}

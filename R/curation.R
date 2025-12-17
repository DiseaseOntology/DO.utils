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
#'
#' @returns The Google Sheet info (`ss`), as a [googlesheets4::sheets_id].
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
curation_template.NULL <- function(ss = NULL, sheet = NULL, ..., nrow = 50) {
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

#' @export
curation_template.obo_data <- function(.data, ss = NULL, sheet = NULL, ...,
                                       n_max = 20) {
    cur_df <- .data |>
        # need smarter indexing... I think
        dplyr::mutate(
            index = dplyr::dense_rank(paste0(.data$predicate, .data$value)),
            .by = "id",
            .before = "id"
        ) |>
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
        unique() |>
        # collapse_col(value) |> # does nothing... probably don't want to collapse
        dplyr::mutate(
            data_type = dplyr::coalesce(
                .sparql_dt_motif[.data$data_type],
                .data$data_type
            ),
            id = dplyr::if_else(duplicated(.data$id), NA_character_, .data$id)
        ) |>
        append_empty_col(curation_cols, order = TRUE)


  class(cur_df) <- c("curation_template", class(cur_df))
  if (is.null(sheet)) sheet <- paste0("curation-", format(Sys.Date(), "%Y%m%d"))
  gs_info <- googlesheets4::write_sheet(cur_df, ss, sheet)

  if (is.null(ss)) ss <- gs_info
  set_curation_validation(cur_df, ss, sheet)

  invisible(gs_info)
}


# helpers --------------------------------------------------------------------

# define expected columns for curation template (in order)
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


#' Set Data Validation for Curation Templates
set_curation_validation <- function(cur_df, ss, sheet) {
    # add data_type validation
    dt_range <- spreadsheet_range(cur_df, "data_type", sheet = sheet)
    range_add_dropdown(ss, dt_range, values = .curation_opts$data_type)

    # add action validation
    action_range <- spreadsheet_range(cur_df, "action", sheet = sheet)
    range_add_dropdown(ss, action_range, values = curation_action)

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
  col_letter <- LETTERS[which(names(.data) == .col)]
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
    row_ends <- c(rows[1], tail(rows, 1)) + n_header
  } else {
    row_ends <- as.integer(stringr::str_split(row_ends, ":")[[1]]) + n_header
  }

  range <- paste0(col_letter, row_ends, collapse = ":")
  if (!is.null(sheet)) {
    range <- paste0(sheet, "!", range)
  }
  range
}
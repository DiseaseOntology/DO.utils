
#' Create a Curation Template
#'
#' Create a curation template in a Google Sheet, optionally including data.
#'
#' @inheritParams googlesheets4::range_write
#' @param sheet (OPTIONAL) The sheet name, as a string. If `NULL` (default), the
#' sheet name will default to "curation-" with today's date appended (formatted
#' as "%Y%m%d"; see [format.Date()]).
#' @param .data Data to add to the curation sheet. If `NULL` (default), an empty
#' curation sheet will be created.
#' @param nrow The number of rows to create in the curation template when
#' `data = NULL` (default: `50`).
#'
#' @returns The Google Sheet info (`ss`), as a [googlesheets4::sheets_id].
#'
#' @export
curation_template <- function(ss = NULL, sheet = NULL, .data = NULL,
                              nrow = 50) {
  cur_cols <- c(
    "iri/curie", "annotation", "value", "remove", "curation_notes", "links",
    "action_notes", "status"
  )

  if (is.null(.data)) {    # create empty curation sheet
    val <- rep(NA, nrow)

    # inspired by https://stackoverflow.com/a/60495352/6938922
    cur_df <- tibble::as_tibble(rlang::rep_named(cur_cols, list(val)))
  } else {
    cur_df <- .data    # NEED TO ADD REFORMATTING HERE!!!
  }

  if (is.null(sheet)) sheet <- paste0("curation-", format(Sys.Date(), "%Y%m%d"))
  gs_info <- googlesheets4::write_sheet(cur_df, ss, sheet)

  # add curation template data validation
  gs_range <- spreadsheet_range(cur_df, "annotation", sheet = sheet)
  range_add_dropdown(ss, gs_range, values = .curation_opts$header)

  # freeze first two columns
  googlesheets4::with_gs4_quiet(
    googlesheets4:::sheet_freeze(ss, sheet = sheet, ncol = 2)
  )

  invisible(gs_info)
}


# helpers --------------------------------------------------------------------

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
    rlange::abort("Exactly one column must be specified in `.col`")
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
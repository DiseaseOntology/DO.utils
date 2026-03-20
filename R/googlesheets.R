# For information about data validation see,
# https://github.com/tidyverse/googlesheets4/blob/main/R/range_add_validation.R

#' Add Data Validation to Google Sheet Range
#'
#' Add data validation to a Google Sheet range.
#'
#' @inheritParams googlesheets4::range_write
#' @param range Cells to apply data validation to. This `range` argument has
#'   important similarities and differences to `range` elsewhere (e.g.
#'   [googlesheets4::range_read()]):
#'   * Similarities: Can be a cell range, using A1 notation ("A1:D3") or using
#'     the helpers in [googlesheets4::cell-specification]. Can combine sheet
#'     name and cell range ("Sheet1!A5:A") or refer to a sheet by name
#'     (`range = "Sheet1"`, although `sheet = "Sheet1"` is preferred for clarity).
#'   * Difference: Can NOT be a named range.
#' @param msg The message to display when the user types in a value that
#' violates the data validation rule. For `range_add_dropdown()`, only displayed
#' if `reject_input` is `TRUE`.
#' @param values The values to use for the dropdown list, as a character vector.
#' @param reject_input Whether to "Reject the input" (default: `TRUE`) if a
#' value violates the data validation rule or "Show a warning" (`FALSE`).
#' @param display_arrow Whether to display a dropdown arrow next to the cell
#' (default: `TRUE`) or not (`FALSE`).
#' @name range_add_validation
NULL

#' @rdname range_add_validation
#' @keywords internal
range_add_checkbox <- function(ss, sheet = NULL, range,
                               msg = "Value must be TRUE or FALSE",
                               quiet = TRUE) {
  rule <- googlesheets4:::new(
    "DataValidationRule",
    condition = googlesheets4:::new_BooleanCondition(type = "BOOLEAN"),
    inputMessage = msg,
    strict = TRUE, # same as in range_add_dropdown(), FALSE doesn't make sense
    showCustomUi = TRUE # seems to be ignored
  )

  if (quiet) {
    .fn <- function(...) {
      googlesheets4::with_gs4_quiet(googlesheets4:::range_add_validation(...))
    }
  } else {
    .fn <- function(...) {
      googlesheets4:::range_add_validation(...)
    }
  }

  .fn(ss = ss, sheet = sheet, range = range, rule = rule)
}

#' @rdname range_add_validation
#' @section Limitations of the Google Sheets API/`googlesheets4`:
#' - The API does not support chipset multi-selection in dropdowns:
#' https://stackoverflow.com/questions/79653536/how-to-enable-multiple-selection-in-data-validation-dropdown-using-google-sheets
#' @keywords internal
range_add_dropdown <- function(ss, sheet = NULL, range, values,
                               msg = "Choose a valid value",
                               reject_input = TRUE, display_arrow = TRUE,
                               quiet = TRUE) {
  rule <- googlesheets4:::new(
    "DataValidationRule",
    condition = googlesheets4:::new_BooleanCondition(
      type = "ONE_OF_LIST",
      values = values
    ),
    inputMessage = msg,
    strict = reject_input,
    showCustomUi = display_arrow
  )

  if (quiet) {
    .fn <- function(...) {
      googlesheets4::with_gs4_quiet(googlesheets4:::range_add_validation(...))
    }
  } else {
    .fn <- function(...) {
      googlesheets4:::range_add_validation(...)
    }
  }

  .fn(ss = ss, sheet = sheet, range = range, rule = rule)
}

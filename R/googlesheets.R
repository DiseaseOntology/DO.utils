# For information about data validation see,
# https://github.com/tidyverse/googlesheets4/blob/main/R/range_add_validation.R

#' Add Data Validation to Google Sheet Range
#'
#' Add data validation to a Google Sheet range.
#'
#' @inheritParams googlesheets4::range_write
#' @param range Cells to apply data validation to. This `range` argument has
#'   important similarities and differences to `range` elsewhere (e.g.
#'   [range_read()]):
#'   * Similarities: Can be a cell range, using A1 notation ("A1:D3") or using
#'     the helpers in [`cell-specification`]. Can combine sheet name and cell
#'     range ("Sheet1!A5:A") or refer to a sheet by name (`range = "Sheet1"`,
#'     although `sheet = "Sheet1"` is preferred for clarity).
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


#' Set Google Sheets Range Fill Color
#'
#' Sets the fill color for one or more ranges in a Google Sheet.
#'
#' @inheritParams curation_template
#' @param ranges A character vector of ranges to set the background color for,
#' as recognized by [googlesheets4::range_flood()].
#'
#' @section NOTE:
#' This function relies on internal functions from the `googlesheets4` package
#' and may break if the package is updated. See examples of
#' [googlesheets4::range_flood()] for reference.
#'
#' @family Google Sheets Formatting functions
#'
#' @keywords internal
set_gs_fill <- function(ss, sheet, ranges, colors) {
  stopifnot(
    "`ranges` and `colors` must be same length" =
      length(ranges) == length(colors),
  )
  color_mat <- gs_col2rgb(colors)

  gs_fill <- purrr::map(
    seq_along(ranges),
    function(.i) {
      googlesheets4:::CellData(
        userEnteredFormat = googlesheets4:::new(
          "CellFormat",
          backgroundColor = googlesheets4:::new(
            "Color",
            red = color_mat[1, .i] / 255,
            green = color_mat[2, .i] / 255,
            blue = color_mat[3, .i] / 255
          )
        )
      )
    }
  )

  purrr::walk2(
    ranges,
    gs_fill,
    ~ googlesheets4::range_flood(ss, sheet, range = .x, cell = .y)
  )
}


#' Convert Google Sheets Colors to RGB
#'
#' Converts one or more Google Sheets color names or hex codes to RGB values.
#'
#' @param colors A character vector of hex codes or one of the following color
#' names from Google Sheets `r paste0(names(gs_color), collapse = ", ")`.
#' @return A 3 x `length(colors)` matrix with `Red`, `Green`, & `Blue` rows in
#' 1 column for each color in `colors`.
#'
#' @family Google Sheets Formatting functions
#'
#' @keywords internal
gs_col2rgb <- function(colors) {
  hex <- is_hex_color(colors)
  stopifnot(
    "`colors` must be one or more hex code(s) or recognized DO.utils:::gs_color name(s)" =
      all(hex | colors %in% names(gs_color))
  )
  color_data <- ifelse(hex, colors, gs_color[colors])

  col2rgb(color_data)
}


# Google Sheets colors defined in DO.utils
gs_color <- c(
  "red" = "#ff0000",
  "orange" = "#ff9900",
  "yellow" = "#ffff00",
  "green" = "#00ff00",
  "cyan" = "#00ffff",
  "blue" = "#0000ff",
  "purple" = "#9900ff",
  "magenta" = "#ff00ff",
  "light red 3" = "#f4cccc",
  "light orange 3" = "#fce5cd",
  "light yellow 3" = "#fff2cc",
  "light green 3" = "#d9ead3",
  "light cyan 3" = "#d0e0e3",
  "light blue 3" = "#cfe2f3",
  "light purple 3" = "#d9d2e9",
  "light magenta 3" = "#ead1dc",
  "white" = "#ffffff",
  "black" = "#000000",
  "dark grey 1" = "#b7b7b7",
  "light grey 2" = "#efefef"
)

#' Convert Column Number to Spreadsheet Column Letter
#'
#' Convert a column numeric position into the corresponding spreadsheet column
#' letter reference (e.g. 1 -> "A", 27 -> "AA").
#'
#' @param x The column number, as a single, positive whole number.
#' @return An spreadsheet column letter, as a string.
#'
#' @section Notes:
#' Modified from a vectorized version created by GPT-5 mini (2025-12-17).
#'
#' @examples
#' colnum_to_ss_letter(1)
#' sapply(c(26, 27, 52, 703), colnum_to_ss_letter)
#'
#' @noRd
colnum_to_ss_letter <- function(x) {
  if (!is_scalar_whole_number(x) || !is_positive(x) || is.na(x)) {
    rlang::abort("`x` must be a single, positive whole number")
  }

  x <- as.integer(x)
  pieces <- character()
  i <- 1
  while (x > 0L) {
    r <- (x - 1L) %% 26L
    pieces <- c(LETTERS[r + 1L], pieces)
    x <- (x - 1L) %/% 26L
  }
  paste0(pieces, collapse = "")
}

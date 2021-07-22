#' Get Current Year
#'
#' Returns the current 4-digit year as a string
#' @export
cur_yr <- function() {
    format(Sys.Date(), "%Y")
}

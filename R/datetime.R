#' Get Current Year
#'
#' Returns the current 4-digit year as a string
#' @export
cur_yr <- function() {
    format(Sys.Date(), "%Y")
}


#' Today's Date Stamp
#'
#' Returns today's date in a format appropriate for use as a date stamp.
#' Format = YMD (Year Month Day) without separators (e.g. "20210728" for
#' July 28, 2021).
#'
#' @export
today_datestamp <- function() {
    format(Sys.Date(), "%Y%m%d")
}

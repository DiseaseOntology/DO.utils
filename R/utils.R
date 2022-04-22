#' Package User Agent (INTERNAL)
#'
#' The user agent for DO.utils. For more info see
#' https://en.wikipedia.org/wiki/User_agent#Use_in_HTTP.
#'
#' @param type The user agent format to use, as a string.
#'
#' @returns
#' For "name", "version" or "full", a string. For "all", a character vector of
#' all other options.
#'
#' @noRd
#' @keywords internal
DO_agent <- function(type = "full") {
    type <- match.arg(type, c("full", "name", "version", "all"))

    .nm <- "DO.utils"
    .vers <- paste0(.nm, "/", as.character(utils::packageVersion("DO.utils")))
    .full <- paste0(.vers, " (https://github.com/allenbaron/DO.utils)")

    switch(
        type,
        full = .full,
        name = .nm,
        version = .vers,
        all = c(name = .nm, version = .vers, full = .full)
    )
}

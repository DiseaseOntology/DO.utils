#' Wrap an On-screen Message
#'
#' Wraps an on-screen message, such as those used in messages, warnings, and
#' errors.
#'
#' @param msg The message to wrap, as a string.
#' @inheritParams base::strwrap
#' @inheritDotParams base::strwrap
#'
#' @examples
#' short_msg <- "This is a short message."
#' long_msg <- "This is a long message to demonstrate how wrap_onscreen() works. It's basically intended to be a multi-line paragraph but, as you can see if you view this variable, it is really just a very long string. We hope you enjoy!"
#'
#' # no effect on short messages
#' message(wrap_onscreen(short_msg))
#'
#' # wrapping of longer messages, has default exdent
#' message(wrap_onscreen(long_msg))
#'
#' # wrapping without exdent
#' message(wrap_onscreen(long_msg, exdent = 0))
#'
#' @export
wrap_onscreen <- function(msg, exdent = 2, ...) {
    paste0(strwrap(msg, exdent = exdent, ...), collapse = "\n")
}

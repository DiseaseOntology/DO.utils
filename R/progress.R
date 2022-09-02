#' Progress Bar (RefClass)
#'
#' Instantiate a base R-style progress bar and advances it by one 'tick' at a
#' time.
#'
#' @field bar A `progress_bar` object created by [utils::txtProgressBar()].
#' @field tick An integer to track progress.
#' @export progress_bar
progress_bar <- methods::setRefClass(
    "progress_bar",
    fields = c("bar", "tick"),
    methods = list(
        initialize = function(n, ...) {
            'Initializes a progress bar when called with \\code{progress_bar$new(n, ...)}.
            \\itemize{
            \\item{\\code{n}}{: Total number of ticks.}
            \\item{\\code{...}}{: Arguments passed on to \\code{\\link[utils::txtProgressBar]{utils::txtProgressBar()}}.}
            }'
            args <- list(...)
            bar <<- utils::txtProgressBar(
                min = 0,
                max = n,
                style = if ("style" %in% names(args)) { args$style } else { 3 },
                width = if ("width" %in% names(args)) { args$width } else { 50 },
                char = if ("char" %in% names(args)) { args$char } else { "=" }
            )
            tick <<- 1L
        },
        advance = function(...) {
            'Advances progress bar one tick.'
            utils::setTxtProgressBar(bar, tick, ...)
            tick <<- tick + 1
        }
    )
)
